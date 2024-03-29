#include "./ptr.mligo"
#include "./kit.mligo"
#include "./tok.mligo"

(* [@@@coverage off] *)

type avl_ptr = AVLPtr of ptr
(* [@@deriving show] *)

type leaf_ptr = LeafPtr of ptr
(* [@@deriving show] *)

type liquidation_slice_contents = {
  burrow: address * nat;
  tok: tok;
  min_kit_for_unwarranted: kit option;
}
(* [@@deriving show] *)

type liquidation_slice = {
  contents: liquidation_slice_contents;
  older: leaf_ptr option;
  younger: leaf_ptr option;
}
(* [@@deriving show] *)

type liquidation_auction_id = avl_ptr
(* [@@deriving show] *)

type bid = { address: address; kit: kit }
(* [@@deriving show] *)

type auction_outcome = {
  sold_tok: tok;
  winning_bid: bid;
  younger_auction: avl_ptr option;
  older_auction: avl_ptr option;
}
(* [@@deriving show] *)

type leaf = {
  value: liquidation_slice;
  parent: ptr;
}
(* [@@deriving show] *)

(* TODO Instead of storing left_height and right_height, we could just
 * store the sum type LeftHeavy | Balanced | RightHeavy. However, I think
 * we might want to leave some trees unbalanced, and I think this approach
 * might work better in that case. *)
type branch = {
  left: ptr;
  left_height: nat;
  left_tok: tok;
  right_tok: tok;
  right_height: nat;
  right: ptr;
  parent: ptr;
}
(* [@@deriving show] *)

type node =
  | Leaf of leaf
  | Branch of branch
  | Root of (ptr option * auction_outcome option)
(* [@@deriving show] *)

type liquidation_auction_bid = { auction_id: liquidation_auction_id; bid: bid; }
(* [@@deriving show] *)

(* [@@@coverage on] *)
