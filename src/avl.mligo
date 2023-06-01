(* Current state
 *
 * The code is verbose, we should figure out some utilities to make it more
 * readable. (especially around setting left and right children of a branches
 * while maintaining the invariants, and we can implement more things in terms
 * of 'join')
 *
 * The invariants of the functions are sometimes not clear. Especially around
 * handling of the parent pointers; some functions overwrite the parent pointer
 * of the return value and expect the caller to set it properly, but some leave
 * it as is. This should be documented per function.
 *
 * Most of the times, when we set a value and return a pointer to it, the caller
 * immediately reads that location again. We can do this more efficiently by
 * returning the value of a pointer alongside with the pointer itself when
 * possible. We choose to skip the optimisation to keep the code clearer.
 *
 * The split function currently splits out "at most" given amount of tokens,
 * however the auction process requires us to split the next element if necessary.
 * This can be implemented by popping the smallest leftover, splitting and
 * re-inserting the pieces to both trees. And we might have some small
 * functions helping this, but it shouldn't be too hard to implement them.
 * For efficiency purposes, we might also end up writing a dedicated function.
 *
 * There are some amount of property tests on major code paths, but there
 * might be other issues.
 *
 * Also, currently all of the operations keep the tree balanced, but we might
 * not want to pay that cost for all operations. For example, when we split
 * a prefix, it might be okay if the new trees are left unbalanced.
*)

(* The AVL tree double-ended queue backed by a doubly-linked balanced
 * tree where the leaves contain the liquidation elements, and the
 * branches contain the amount of collateral on their left and right children.
 *
 * It acts as a mutable data structure implemented on top of a bigmap
 * (just a Map in Ocaml prototype), acting as a "memory". So, the caller
 * should thread the memory across function calls. Returned objects from
 * AVL operations are only valid on the returned `mem` objects.
 *
 * The tree consists of three different nodes, where two of them, "root"
 * and "leaf" has dedicated pointer types called `avl_ptr` and `leaf_ptr`.
 * These pointers are "stable" references, where the operations on the
 * tree does not move them. (This is not the case with branches, where
 * any operation can create/update/delete any branches, so you can not
 * refer to them safely).
 *
 * This structure has an efficient (log(n)) `append` function (ergo,
 * `push_back` and `push_front` functions. Also, given a reference
 * to a leaf, it is possible to delete it `efficiently`.
*)

#import "./mem.mligo" "Mem"
#import "./ptr.mligo" "Ptr"
#import "./liquidationAuctionPrimitiveTypes.mligo" "Liquidation"
#import "./common.mligo" "Common"
#import "./tok.mligo" "Tok"
#include "./error.mligo"

[@inline] let ptr_of_avl_ptr (ptr: Liquidation.avl_ptr) = match ptr with AVLPtr r -> r

let node_tok (n: Liquidation.node) : Tok.tok =
  match n with
  | Leaf leaf -> leaf.value.contents.tok
  | Branch branch -> Tok.tok_add branch.left_tok branch.right_tok
  | Root _ -> (failwith internalError_NodeTezFoundRoot : Tok.tok)

let node_height (n: Liquidation.node) : nat =
  match n with
  | Leaf _ -> 1n
  | Branch branch ->
    let max = if branch.left_height > branch.right_height
      then branch.left_height else branch.right_height in
    max + (1n)
  | Root _ -> (failwith internalError_NodeHeightFoundRoot : nat)

[@inline] let node_parent (n: Liquidation.node) : Ptr.ptr =
  match n with
  | Leaf leaf -> leaf.parent
  | Branch branch -> branch.parent
  | Root _ -> (failwith internalError_NodeParentFoundRoot : Ptr.ptr)

[@inline] let node_branch (n: Liquidation.node) : Liquidation.branch =
  match n with
  | Branch branch -> branch
  | _ -> (failwith internalError_NodeBranchFoundNonBranch : Liquidation.branch)

[@inline] let node_leaf (n: Liquidation.node) : Liquidation.leaf =
  match n with
  | Leaf leaf -> leaf
  | _ -> (failwith internalError_NodeLeafFoundNonLeaf : Liquidation.leaf)

let deref_avl_ptr (mem, p: Mem.mem * Liquidation.avl_ptr): Ptr.ptr option * Liquidation.auction_outcome option =
  let p = match p with AVLPtr p -> p in
  match Mem.mem_get (mem, p) with
  | Root p -> p
  | _ -> (failwith internalError_DerefAvlPtrFoundNonRoot : Ptr.ptr option * Liquidation.auction_outcome option)

[@inline] let deref_leaf_ptr (mem, p: Mem.mem * Liquidation.leaf_ptr): Liquidation.leaf =
  match p with LeafPtr p -> node_leaf (Mem.mem_get (mem, p))

[@inline] let node_left (n: Liquidation.node) : Ptr.ptr =
  let b = node_branch n in b.left

[@inline] let node_right (n: Liquidation.node) : Ptr.ptr =
  let b = node_branch n in b.right

[@inline] let node_set_parent (p: Ptr.ptr) (n: Liquidation.node) : Liquidation.node =
  match n with
  | Leaf leaf -> Leaf { leaf with parent = p; }
  | Branch branch -> Branch { branch with parent = p; }
  | Root _ -> (failwith internalError_NodeSetParentFoundRoot : Liquidation.node)

let update_matching_child
    (mem, ptr, from_ptr, to_ptr: Mem.mem * Ptr.ptr * Ptr.ptr * Ptr.ptr) : Mem.mem =
  match Mem.mem_get (mem, ptr) with
  | Root (_b, r) ->

    Mem.mem_set (mem, ptr, Root ((Some to_ptr), r))
  | Leaf _ ->
    (failwith internalError_UpdateMatchingChildFoundLeaf : Mem.mem)
  | Branch old_branch ->
    let to_ = Mem.mem_get (mem, to_ptr) in
    let new_branch =
      if old_branch.left = from_ptr
      then Branch {
          old_branch with
          left = to_ptr;
          left_tok = node_tok to_;
          left_height = node_height to_;
        }
      else (

        Branch {
          old_branch with
          right = to_ptr;
          right_tok = node_tok to_;
          right_height = node_height to_;
        }) in
    Mem.mem_set (mem, ptr, new_branch)

(*
 * Operations on AVL trees.
 *
 * The resulting 'ptr' and 'mem' should always be used together. They are
 * not as part of a product type, because a 'mem' can carry multiple trees,
 * in case of 'take'.
 *
 * The operations do not move leaves or the root, so pointers to them are stable
 * (unless a leaf is deleted with 'del').
 *
 * Implementation detail: The functions prefixed by `ref_` work on untyped
 * `ptr` pointers, and usually are only passed branches (not root). There
 * are usually wrappers around them which makes them work on `avl_ptr`'s
 * via doing the necessary unwrapping. The reason of this distinction is
 * that tree operations are usually called recursively and they don't have
 * a concept of a `Root` node.
 *)

let avl_mk_empty (mem: Mem.mem) (r: Liquidation.auction_outcome option): Mem.mem * Liquidation.avl_ptr =
  let (mem, ptr) = Mem.mem_new (mem, (Root ((None: Ptr.ptr option), r))) in
  (mem, AVLPtr ptr)

(* Before:
 *
 *            curr
 *            /  \
 *           /    \
 *         left  right
 *               /   \
 *              /     \
 *         right_left  a
 *
 * After:
 *
 *            right
 *            /   \
 *           /     \
 *        curr      a
 *        /   \
 *       /     \
 *     left  right_left
*)
let ref_rotate_left (mem, curr_ptr: Mem.mem * Ptr.ptr) : Mem.mem * Ptr.ptr =
  let curr =
    match Mem.mem_get (mem, curr_ptr) with
    | Branch curr -> curr
    | _ -> (failwith internalError_RefRotateLeftCurrentPtrNotBranch : Liquidation.branch) in

  let right_ptr = curr.right in
  let right =
    match Mem.mem_get (mem, right_ptr) with
    | Branch right -> right
    | _ -> (failwith internalError_RefRotateLeftRightPtrNotBranch : Liquidation.branch) in

  let right_left_ptr = right.left in

  (* move right_left under curr *)
  let mem = Mem.mem_update (mem, right_left_ptr, (node_set_parent curr_ptr)) in
  let mem = update_matching_child (mem, curr_ptr, right_ptr, right_left_ptr) in

  (* move curr under right *)
  let mem = Mem.mem_update (mem, right_ptr, (node_set_parent curr.parent)) in
  let mem = Mem.mem_update (mem, curr_ptr, (node_set_parent right_ptr)) in
  let mem = update_matching_child (mem, right_ptr, right_left_ptr, curr_ptr) in

  (mem, right_ptr)

(* Before:
 *
 *            curr
 *            /  \
 *           /    \
 *         left  right
 *         / \
 *        /   \
 *       a  left_right
 *
 * After:
 *
 *             left
 *             /  \
 *            /    \
 *           a    curr
 *               /    \
 *              /      \
 *        left_right   right
*)
let ref_rotate_right (mem, curr_ptr: Mem.mem * Ptr.ptr) : Mem.mem * Ptr.ptr =
  let curr =
    match Mem.mem_get (mem, curr_ptr) with
    | Branch curr -> curr
    | _ -> (failwith internalError_RefRotateRightCurrentPtrNotBranch : Liquidation.branch) in

  let left_ptr = curr.left in
  let left =
    match Mem.mem_get (mem, left_ptr) with
    | Branch left -> left
    | _ -> (failwith internalError_RefRotateRightLeftPtrNotBranch : Liquidation.branch) in

  let left_right_ptr = left.right in

  (* move left_right under curr *)
  let mem = Mem.mem_update (mem, left_right_ptr, (node_set_parent curr_ptr)) in
  let mem = update_matching_child (mem, curr_ptr, left_ptr, left_right_ptr) in

  (* move curr under left *)
  let mem = Mem.mem_update (mem, left_ptr, (node_set_parent curr.parent)) in
  let mem = Mem.mem_update (mem, curr_ptr, (node_set_parent left_ptr)) in
  let mem = update_matching_child (mem, left_ptr, left_right_ptr, curr_ptr) in

  (mem, left_ptr)

(* From: https://en.wikipedia.org/wiki/Avl_tree#Rebalancing
 *
 * Dir1  | Dir2
 * ------+------
 * Left  | Left  => Z is a left  child of its parent X and Z is not right-heavy
 * Left  | Right => Z is a left  child of its parent X and Z is right-heavy
 * Right | Right => Z is a right child of its parent X and Z is not left-heavy
 * Right | Left  => Z is a right child of its parent X and Z is left-heavy
 *
 * The balance violation of case Dir1 = Dir2 is repaired by
 *   a simple rotation: rotate_(−Dir1)
 * The case Dir1 <> Dir2 is repaired by
 *   a double rotation: rotate_Dir1Dir2
*)
let rebalance (mem, curr_ptr: Mem.mem * Ptr.ptr) : Mem.mem * Ptr.ptr =
  match Mem.mem_get (mem, curr_ptr) with
  | Branch branch ->
    if (abs (branch.left_height - branch.right_height)) > (1n) then (
      let diff = branch.right_height - branch.left_height in
      let heavy_child_ptr =
        if diff < 0 then branch.left else branch.right in
      let heavy_child = match Mem.mem_get (mem, heavy_child_ptr) with
        | Branch b -> b
        | _ -> (failwith internalError_RebalanceHeavyChildNonBranchch : Liquidation.branch) in
      let heavy_child_balance =
        heavy_child.right_height - heavy_child.left_height in
      let (mem, ptr) =
        if diff < 0 then (
          (* (diff < 0) *)
          if heavy_child_balance <= 0 then
            (* (hcb <= 0) *)
            (* Left, Left *)
            ref_rotate_right (mem, curr_ptr)
          else (
            (* (hcb > 0) *)
            (* Left, Right *)

            let (mem, new_) = ref_rotate_left (mem, heavy_child_ptr) in
            let mem = update_matching_child (mem, curr_ptr, heavy_child_ptr, new_) in
            ref_rotate_right (mem, curr_ptr)
          )
        )
        else (
          (* (diff >= 0) *)

          if heavy_child_balance >= 0 then
            (* (hcb >= 0) *)
            (* Right, Right *)
            ref_rotate_left (mem, curr_ptr)
          else (
            (* (hcb < 0) *)
            (* Right, Left *)

            let (mem, new_) = ref_rotate_right (mem, heavy_child_ptr) in
            let mem = update_matching_child (mem, curr_ptr, heavy_child_ptr, new_) in
            ref_rotate_left (mem, curr_ptr)
          )
        )
      in

      (mem, ptr)
    ) else (mem, curr_ptr)
  | Leaf _ -> (mem, curr_ptr)
  | Root _ -> (mem, curr_ptr)

type direction =
  | Left
  | Right

(* ************************** *)

type ref_join_data = {
  join_direction: direction;
  ptr: Ptr.ptr;
  to_fix: Ptr.ptr;
  parent_ptr: Ptr.ptr;
}

let ref_join_post_processing
    (data: ref_join_data)
    ((mem, new_child) : Mem.mem * Ptr.ptr)
  : Mem.mem * Ptr.ptr =
  let mem = update_matching_child (mem, data.ptr, data.to_fix, new_child) in
  let (mem, new_tree) = rebalance (mem, data.ptr) in
  let mem = Mem.mem_update (mem, new_tree, (node_set_parent data.parent_ptr)) in

  (mem, new_tree)

(* Nice and tail-recursive left fold we can write in ligo more or less as-is. *)
let rec left_fold_ref_join_data
    ((mem_and_child_ptr, stack): (Mem.mem * Ptr.ptr) * ref_join_data list)
  : Mem.mem * Ptr.ptr =
  match stack with
  | [] -> mem_and_child_ptr
  | d :: ds ->
    let new_mem_and_child_ptr = ref_join_post_processing d mem_and_child_ptr in
    left_fold_ref_join_data (new_mem_and_child_ptr, ds)

(* Appends left_ptr and right_ptr to form a new tree, and returns a pointer to
 * the newly created tree. The resulting node will inherit the parent of the
 * "${join_direction}_ptr". *)
let rec ref_join_rec
    (mem, join_direction, left_ptr, right_ptr, stack: Mem.mem * direction * Ptr.ptr * Ptr.ptr * ref_join_data list)
  : Mem.mem * Ptr.ptr =
  let left = Mem.mem_get (mem, left_ptr) in
  let right = Mem.mem_get (mem, right_ptr) in

  (* The given direction determines whose parent will be the parent of the
   * resulting tree. *)
  let parent_ptr = match join_direction with
    | Left -> node_parent left
    | Right -> node_parent right
  in

  (* If the left and right is of similar height, simply combining them
   * as a branch gives a valid AVL. *)
  if
      (abs ((node_height left) - (node_height right)))
      <
      2n then
    let new_branch = Branch {
        left = left_ptr;
        left_height = node_height left;
        left_tok = node_tok left;
        right_tok = node_tok right;
        right_height = node_height right;
        right = right_ptr;
        parent = parent_ptr;
      } in

    let (mem, ptr) = Mem.mem_new (mem, new_branch) in
    let mem = Mem.mem_update (mem, left_ptr, (node_set_parent ptr)) in
    let mem = Mem.mem_update (mem, right_ptr, (node_set_parent ptr)) in

    (* Do all the patching up here *)
    left_fold_ref_join_data ((mem, ptr), stack)
  else
    let new_join_direction, left_p, right_p, (ptr, to_fix) =
      (* If the left is heavier, we can make left the parent and append the
       * original right to left.right . *)
      if (node_height left) > (node_height right) then
        let left_p = node_right left in
        (Left, left_p, right_ptr, (left_ptr, left_p))
        (* Or vice versa. *)
      else (* node_height left < node_height right *)
        let right_p = node_left right in
        (Right, left_ptr, right_p, (right_ptr, right_p))
    in
    ref_join_rec
      ( mem
      , new_join_direction
      , left_p
      , right_p
      , ({ join_direction=join_direction; ptr=ptr; to_fix=to_fix; parent_ptr=parent_ptr; } :: stack)
      )

let ref_join
    (mem, join_direction, left_ptr, right_ptr: Mem.mem * direction * Ptr.ptr * Ptr.ptr) : Mem.mem * Ptr.ptr =
  ref_join_rec
    ( mem
    , join_direction
    , left_ptr
    , right_ptr
    , ([]: ref_join_data list)
    )

(* ************************** *)

(* Left for pushing back and Right for pushing front *)
let avl_push
    (mem: Mem.mem) (root_ptr: Liquidation.avl_ptr) (value: Liquidation.liquidation_slice) (d: direction)
  : Mem.mem * Liquidation.leaf_ptr =
  let (root, root_data) = deref_avl_ptr (mem, root_ptr) in
  let root_ptr = match root_ptr with AVLPtr r -> r in

  let node = Leaf { value=value; parent=root_ptr; } in
  let (mem, leaf_ptr) = Mem.mem_new (mem, node) in

  begin match root with
    (* When the tree is empty, create the initial leaf. *)
    | None ->
      let mem = Mem.mem_set (mem, root_ptr, Root (Some leaf_ptr, root_data)) in
      (mem, LeafPtr leaf_ptr)
    (* When there is already an element, join with the new leaf. *)
    | Some ptr ->
      let (mem, ret) =
        begin match d with
          | Left -> ref_join (mem, Left, ptr, leaf_ptr)
          | Right -> ref_join (mem, Right, leaf_ptr, ptr)
        end in
      let mem = Mem.mem_set (mem, root_ptr, Root (Some ret, root_data)) in
      (mem, LeafPtr leaf_ptr)
  end

(* Pushes the provided value to the back of the queue *)
[@inline] let avl_push_back (mem: Mem.mem) (root_ptr: Liquidation.avl_ptr) (value: Liquidation.liquidation_slice) =
  avl_push mem root_ptr value Left

(* Pushes the provided value to the front of the queue *)
[@inline] let avl_push_front (mem: Mem.mem) (root_ptr: Liquidation.avl_ptr) (value: Liquidation.liquidation_slice) =
  avl_push mem root_ptr value Right

let rec balance_bottom_up ((mem, curr_ptr): Mem.mem * Ptr.ptr): Mem.mem * Liquidation.avl_ptr =
  let curr = Mem.mem_get (mem, curr_ptr) in
  match curr with
  | Root _ -> (mem, AVLPtr curr_ptr)
  | Leaf _ -> (failwith internalError_BalanceBottomUpFoundLeaf : Mem.mem * Liquidation.avl_ptr)
  | Branch b ->
    (* TODO we can stop recursing up when node height does not change. *)
    let (mem, new_curr) = rebalance (mem, curr_ptr) in

    let mem = update_matching_child (mem, b.parent, curr_ptr, new_curr) in
    balance_bottom_up (mem, b.parent)

(* Deletes a leaf pointer. Note that this does not require the tree root
 * to be passed. Returns the root of the tree as an extra information. *)
let ref_del (mem, ptr: Mem.mem * Ptr.ptr): Mem.mem * Liquidation.avl_ptr =
  let self = Mem.mem_get (mem, ptr) in
  let parent_ptr = node_parent self in
  let mem = Mem.mem_del (mem, ptr) in
  match Mem.mem_get (mem, parent_ptr) with
  | Leaf _ -> (failwith internalError_RefDelParentIsLeaf : Mem.mem * Liquidation.avl_ptr)
  (* when deleting the sole element, we return an empty tree *)
  | Root r ->
    let (_, m) = r in
    let mem = Mem.mem_set (mem, parent_ptr, Root ((None: Ptr.ptr option), m)) in
    (mem, AVLPtr parent_ptr)
  (* otherwise, the parent of the deleted element is redundant since it
   * only has a single child, so we delete the parent and the orphan sibling
   * is adopted by the grandparent who have lost its child. *)
  | Branch parent ->
    let sibling_ptr = if parent.left = ptr
      then parent.right
      else parent.left in

    let mem = Mem.mem_del (mem, parent_ptr) in
    let grandparent_ptr = parent.parent in
    let mem = Mem.mem_update (mem, sibling_ptr, (node_set_parent grandparent_ptr)) in
    let mem = update_matching_child
        (mem,
        grandparent_ptr,
        parent_ptr,
        sibling_ptr) in
    balance_bottom_up (mem, grandparent_ptr)

let avl_del (mem, ptr: Mem.mem * Liquidation.leaf_ptr): Mem.mem * Liquidation.avl_ptr =
  match ptr with LeafPtr ptr -> ref_del (mem, ptr)

let avl_read_leaf (mem, ptr: Mem.mem * Liquidation.leaf_ptr): Liquidation.liquidation_slice =
  let l = deref_leaf_ptr (mem, ptr) in
  l.value

let avl_update_leaf (mem, ptr, f: Mem.mem * Liquidation.leaf_ptr * (Liquidation.liquidation_slice -> Liquidation.liquidation_slice)): Mem.mem =
  let l = deref_leaf_ptr (mem, ptr) in
  let ptr = match ptr with LeafPtr p -> p in
  Mem.mem_set (mem, ptr, Leaf { l with value = f l.value })

let avl_is_empty (mem, ptr: Mem.mem * Liquidation.avl_ptr) : bool =
  let (r, _) = deref_avl_ptr (mem, ptr) in
  (match r with | None -> true | Some _ -> false)

let avl_delete_empty_tree (mem, ptr: Mem.mem * Liquidation.avl_ptr): Mem.mem =
  let (r, _) = deref_avl_ptr (mem, ptr) in
  match r with
  | Some _ -> (failwith internalError_AvlDeleteEmptyTreeNonEmptyTree : Mem.mem)
  | None -> Mem.mem_del (mem, (ptr_of_avl_ptr ptr))

let avl_find_root (mem, leaf: Mem.mem * Liquidation.leaf_ptr) : Liquidation.avl_ptr =
  let leaf = match leaf with LeafPtr p -> p in
  let rec go (ptr: Liquidation.ptr) : Liquidation.avl_ptr =
    match Mem.mem_get (mem, ptr) with
    | Root _ -> AVLPtr ptr
    | Branch b -> go b.parent
    | Leaf l -> go l.parent in
  go leaf

let rec ref_peek_front (mem, ptr: Mem.mem * Ptr.ptr) : Liquidation.leaf_ptr * Liquidation.leaf =
  let self = Mem.mem_get (mem, ptr) in
  match self with
  | Leaf l -> (LeafPtr ptr, l)
  | Branch b -> ref_peek_front (mem, b.left)
  | Root _ -> (failwith internalError_RefPeekFrontFoundRoot : Liquidation.leaf_ptr * Liquidation.leaf)

let avl_peek_front (mem, ptr: Mem.mem * Liquidation.avl_ptr) : (Liquidation.leaf_ptr * Liquidation.leaf) option =
  let (p, _) = deref_avl_ptr (mem, ptr) in
  match p with
  | None -> (None: (Liquidation.leaf_ptr * Liquidation.leaf) option)
  | Some r -> Some (ref_peek_front (mem, r))

(* FIXME: needs an efficient reimplementation *)
let avl_pop_front (mem: Mem.mem) (root_ptr: Liquidation.avl_ptr) : Mem.mem * (Liquidation.leaf_ptr * Liquidation.liquidation_slice) option =
  let (r, _) = deref_avl_ptr (mem, root_ptr) in
  match r with
  | None -> (mem, (None: (Liquidation.leaf_ptr * Liquidation.liquidation_slice) option))
  | Some r ->
    let (leafptr, leaf) = ref_peek_front (mem, r) in
    let (mem, _) = avl_del (mem, leafptr) in
    (mem, Some (leafptr, leaf.value))

(* ************************** *)

type ref_split_data = {
  rec_direction: direction;
  branch: Liquidation.branch;
}

let ref_split_post_processing
    (data : ref_split_data)
    ((mem, maybe_left, maybe_right) : Mem.mem * Ptr.ptr option * Ptr.ptr option)
  : Mem.mem * Ptr.ptr option * Ptr.ptr option =
  let { rec_direction=rec_direction; branch=branch; } = data in
  match rec_direction with
  | Left -> (
      match maybe_right with
      | None -> (failwith internalError_RefSplitPostProcessingInvariantFailed : Mem.mem * Ptr.ptr option * Ptr.ptr option)
      | Some right ->
        let (mem, joined) = ref_join (mem, Right, right, branch.right) in
        (mem, maybe_left, Some joined)
    )
  | Right -> (
      match maybe_left with
      | Some left ->
        let (mem, joined) = ref_join (mem, Left, branch.left, left) in
        (mem, Some joined, maybe_right)
      | None ->
        (mem, Some branch.left, maybe_right)
    )

(* Nice and tail-recursive left fold we can write in ligo more or less as-is. *)
let rec left_fold_ref_split_data
    (mem_and_left_ptr_and_right_ptr, stack : (Mem.mem * Ptr.ptr option * Ptr.ptr option) * ref_split_data list)
  : Mem.mem * Ptr.ptr option * Ptr.ptr option =
  match stack with
  | [] -> mem_and_left_ptr_and_right_ptr
  | d :: ds ->
    let new_mem_and_left_ptr_and_right_ptr = ref_split_post_processing d mem_and_left_ptr_and_right_ptr in
    left_fold_ref_split_data (new_mem_and_left_ptr_and_right_ptr, ds)

(* George: This does not split leaves; if the collateral on the leaf exceeds
 * the limit then it is not included in the result (it's part of the second
 * tree returned). Essentially this means that the union of the resulting trees
 * has the same contents as the input tree. *)
let rec ref_split_rec
    (mem, curr_ptr, limit, stack: Mem.mem * Ptr.ptr * Tok.tok * ref_split_data list)
  : Mem.mem * Ptr.ptr option * Ptr.ptr option =
  match Mem.mem_get (mem, curr_ptr) with
  | Root _ -> (failwith internalError_RefSplitRecFoundRoot : Mem.mem * Ptr.ptr option * Ptr.ptr option)
  | Leaf leaf ->
    if leaf.value.contents.tok <= limit
    then
      (* Case 1a. Single leaf with not too much collateral in it. Include it. *)
      let mem = Mem.mem_update (mem, curr_ptr, (node_set_parent Ptr.ptr_null)) in
      left_fold_ref_split_data ((mem, Some curr_ptr, (None: Ptr.ptr option)), stack)
    else
      (* Case 1b. Single leaf with too much collateral in it. Exclude it. *)
      left_fold_ref_split_data ((mem, (None: Ptr.ptr option), Some curr_ptr), stack)
  | Branch branch ->
    if (Tok.tok_add branch.left_tok branch.right_tok) <= limit
    then (* total_collateral <= limit *)
      (* Case 2. The whole tree has not too much collateral in it. Include it. *)
      let mem = Mem.mem_update (mem, curr_ptr, (node_set_parent Ptr.ptr_null)) in
      left_fold_ref_split_data ((mem, Some curr_ptr, (None: Ptr.ptr option)), stack)
    else (* limit < total_collateral *)
      let mem = Mem.mem_del (mem, curr_ptr) in
      let mem = Mem.mem_update (mem, branch.right, (node_set_parent branch.parent)) in
      (* Semantically it would be better to detach branch.left as well here
       *
       *   let mem = mem_update mem branch.left (node_set_parent Ptr.ptr_null) in
       *
       * instead of changing the parent of branch.left in function "take" below.
       * Unfortunately, this bumps reads and writes significantly (reads+=16%
       * and writes+=20%), so we don't do it. *)

      if branch.left_tok = limit
      then (* Case 3a. left_tok = limit (no need for recursion, split the tree right here) *)
        left_fold_ref_split_data ((mem, Some branch.left, Some branch.right), stack)
      else
        let rec_direction, tree_to_recurse_into, limit_to_use =
          if limit < branch.left_tok
          then (* Case 3b. limit < left_tok < total_collateral (we have to recurse into and split the left tree) *)
            (Left, branch.left, limit)
          else (* Case 3c. left_tok < limit < total_collateral (we have to recurse into and split the right tree) *)
            let left_branch = Mem.mem_get (mem, branch.left) in
            (Right, branch.right, abs (limit - (node_tok left_branch)))
        in
        ref_split_rec
          ( mem
          , tree_to_recurse_into
          , limit_to_use
          , ({ rec_direction=rec_direction; branch=branch } :: stack)
          )

let ref_split
    (mem: Mem.mem)
    (curr_ptr: Ptr.ptr)
    (limit: Tok.tok)
  : Mem.mem * Ptr.ptr option * Ptr.ptr option =
  ref_split_rec
    ( mem
    , curr_ptr
    , limit
    , ([]: ref_split_data list)
    )

(* ************************** *)

(* Split the longest prefix of the tree with less than given amount of
 * collateral. *)
let avl_take (mem: Mem.mem) (root_ptr: Liquidation.avl_ptr) (limit: Tok.tok) (root_data: Liquidation.auction_outcome option)
  : Mem.mem * Liquidation.avl_ptr =
  let (r, old_root_data) = deref_avl_ptr (mem, root_ptr) in
  let root_ptr = match root_ptr with AVLPtr r -> r in
  match r with
  | Some r ->
    let (mem, l, r) = ref_split mem r limit in
    let (mem, new_root) = Mem.mem_new (mem, (Root (l, root_data))) in
    let mem = match l with
      | Some l -> Mem.mem_update (mem, l, (node_set_parent new_root))
      | None -> mem in
    let mem = Mem.mem_set (mem, root_ptr, Root (r, old_root_data)) in
    (mem, AVLPtr new_root)
  | None ->
    let (mem, new_root) = Mem.mem_new (mem, (Root ((None: Ptr.ptr option), root_data))) in
    (mem, AVLPtr new_root)

[@inline] let avl_tok (mem: Mem.mem) (ptr: Liquidation.avl_ptr) : Tok.tok =
  let (r, _) = deref_avl_ptr (mem, ptr) in
  match r with
  | Some ptr -> node_tok (Mem.mem_get (mem, ptr))
  | None -> Tok.tok_zero

[@inline] let avl_height (mem: Mem.mem) (ptr: Liquidation.avl_ptr): nat =
  let (r, _) = deref_avl_ptr (mem, ptr) in
  match r with
  | Some ptr -> node_height (Mem.mem_get (mem, ptr))
  | None -> 0n

[@inline] let avl_root_data (mem, ptr: Mem.mem * Liquidation.avl_ptr) : Liquidation.auction_outcome option =
  let (_, d) = deref_avl_ptr (mem, ptr) in d

[@inline] let avl_modify_root_data (mem, ptr, f: Mem.mem * Liquidation.avl_ptr * (Liquidation.auction_outcome option -> Liquidation.auction_outcome option)) =
  let (r, d) = deref_avl_ptr (mem, ptr) in
  let ptr = match ptr with AVLPtr p -> p in
  Mem.mem_set (mem, ptr, Root (r, f d))
