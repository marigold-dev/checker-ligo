#include "./liquidationAuctionPrimitiveTypes.mligo"
#include "./liquidationAuctionTypes.mligo"
#import "./constants.mligo" "Constants"
#import "./common.mligo" "Common"
#import "./sliceList.mligo" "SL"
#import "./tok.mligo" "Tok"
#import "./kit.mligo" "Kit"
#import "./fixedPoint.mligo" "FixedPoint"

(*
Utku: Lifecycle of liquidation slices.

1. When a 'liquidation_slice' is created from a burrow, it is
   added to the 'queued_slices' queue. This is backed by an AVL
   tree.

   Every 'liquidation_slice' has pointers to the older and younger
   slice for that burrow. This forms a double-linked list of slices
   overlaying the AVL tree.

   Every burrow has pointers to the first and the last element of
   that linked list, so adding/popping elements both  from the front
   and the back is efficient.

2. When checker is touched, and there is no existing auction
   going on, a prefix of the 'queued_slices' is split, and inserted
   as 'current_auction' alongside with the current timestamp. This
   process is efficient because of the AVL tree.

   When the prefix returned does not have as much collateral as we
   want, this means that the next liquidation_slice in 'queued_slices'
   needs to be split. We can do this by popping the first item if exists,
   which as an invariant has more collateral than we need, and splitting
   it into two slices, appending the first slice to the end of
   'current_auction', and the second one to back to the front of
   'queued_slices'. If there is no more slices, we still start the
   auction.

   While doing this, we also need to make sure that the pointers on
   liquidation slices are still correct.

3. Then an auction starts:

   > If there are any lots waiting to be auctioned, Checker starts an open,
   > ascending bid auction. There is a reserve price set at tz_t which declines
   > exponentially over time as long as no bid as been placed. Once a bid
   > is placed, the auction continues. Every bid needs to improve over the
   > previous bid by at least 0.33 cNp and adds the longer of 20 blocks or
   > 20 minutes, to the time before the auction expires.

   All bids can be claimed, unless they are the winning bid of the current
   or any of the past auctions.

   When an auction expires, the current auction is both moved to
   'completed_auctions' FIFO queue. This FIFO queue is implemented as a doubly
   linked list at tree roots.

4. When 'touch_liquidation_slice' is called the slice is checked if it belongs
   to a completed auction or not. This can be checked via the 'find_root'
   function of AVL trees. If we end up in 'queued_slices' or 'curent_auction',
   it is not complete, if not, it means that we're in 'completed_auctions'.

   If the slice is completed, then the burrows outstanding kit balance is
   adjusted and the leaf is deleted from the tree. If it was the last remaining
   slice of the relevant auction (in other words, if the auction's tree becomes
   empty after the removal), the auctions is popped from the 'completed_auctions'
   linked list.

5. When checker is touched, it fetches the oldest auction from `completed_auctions`
   queue, and processes a few of oldest liquidations. This is to clean-up the slices
   that haven't been claimed for some reason.

6. When the winner of an auction tries to claim the result, we check if its auction
   has no liquidation_slice's left. If all the slices are already touched, this
   means that the tree is already popped out of the completed_auctions list. In
   this case, we transfer the result to the callee, and remove the auction alltogether.
*)


(* When burrows send a liquidation_slice, they get a pointer into a tree leaf.
 * Initially that node belongs to 'queued_slices' tree, but this can change over time
 * when we start auctions.
*)
let liquidation_auction_send_to_auction
    (auctions: liquidation_auctions) (contents: liquidation_slice_contents)
  : (liquidation_auctions * leaf_ptr) =
  if (avl_height auctions.avl_storage auctions.queued_slices) >= Constants.max_liquidation_queue_height then
    (failwith error_LiquidationQueueTooLong : liquidation_auctions * leaf_ptr)
  else
    let burrow_slices = SL.slice_list_from_auction_state (auctions, contents.burrow) in
    let auctions, burrow_slices, (SliceListElement (ret, _)) = SL.slice_list_append burrow_slices auctions auctions.queued_slices QueueBack contents in
    let auctions = SL.slice_list_to_auction_state (auctions, burrow_slices) in

    (auctions, ret)

(** Split a liquidation slice into two. The first of the two slices is the
  * "older" of the two (i.e. it is the one to be included in the auction we are
  * starting).
  *
  * We also have to split the min_kit_for_unwarranted so that we can evaluate
  * the two auctions separately (and see if the liquidation was warranted,
  * retroactively). Currently, perhaps a bit harshly, we round up for both
  * slices.  Alternatively, we can calculate min_kit_for_unwarranted_1 and then
  * calculate min_kit_for_unwarranted_2 = min_kit_for_unwarranted -
  * min_kit_for_unwarranted_1. *)
let split_liquidation_slice_contents (amnt: tok) (contents: liquidation_slice_contents) : (liquidation_slice_contents * liquidation_slice_contents) =
  let { burrow = contents_burrow;
        tok = contents_tok;
        min_kit_for_unwarranted = contents_min_kit_for_unwarranted;
      } = contents in


  let slice_tok = tok_to_denomination_int contents_tok in
  (* tok partitioning *)
  let ltok = amnt in
  let rtok = tok_sub contents_tok amnt in
  (* kit partitioning *)
  let (lkit, rkit) =
    match contents_min_kit_for_unwarranted with
    | None -> ((None: kit option), (None: kit option))
    | Some contents_min_kit_for_unwarranted ->
      let min_kit_for_unwarranted = kit_to_denomination_nat contents_min_kit_for_unwarranted in
      let lkit =
        kit_of_fraction_ceil
          (min_kit_for_unwarranted * (tok_to_denomination_int ltok))
          (kit_scaling_factor_int * slice_tok)
      in
      let rkit =
        kit_of_fraction_ceil
          (min_kit_for_unwarranted * (tok_to_denomination_int rtok))
          (kit_scaling_factor_int * slice_tok)
      in
      (Some lkit, Some rkit) in
  ( { burrow = contents_burrow; tok = ltok; min_kit_for_unwarranted = lkit; },
    { burrow = contents_burrow; tok = rtok; min_kit_for_unwarranted = rkit; }
  )

let take_with_splitting (auctions: liquidation_auctions) (split_threshold: tok) : liquidation_auctions * avl_ptr (* liquidation_auction_id *) =
  let queued_slices = auctions.queued_slices in

  let (storage, new_auction) = avl_take auctions.avl_storage queued_slices split_threshold (None: auction_outcome option) in
  let auctions = { auctions with avl_storage = storage } in

  let queued_amount = avl_tok storage new_auction in
  let auctions = if lt_tok_tok queued_amount split_threshold
    then
      (* split next thing *)
      let next = SL.slice_list_from_queue_head auctions in
      match next with
      (* Case: there is a slice to split *)
      | Some (element, burrow_slices) ->
        (* Split the contents *)
        let (part1_contents, part2_contents) =
          split_liquidation_slice_contents (tok_sub split_threshold queued_amount) (SL.slice_list_element_contents element) in
        (* Remove the element we are splitting *)
        let auctions, burrow_slices, _, _ = SL.slice_list_remove (burrow_slices, auctions, element) in
        (* Push the first portion of the slice to the back of the new auction *)
        let auctions, burrow_slices, _ = SL.slice_list_append burrow_slices auctions new_auction QueueBack part1_contents in
        (* Push the remainder of the slice to the front of the auction queue *)
        let auctions, burrow_slices, _ = SL.slice_list_append burrow_slices auctions queued_slices QueueFront part2_contents in
        (* Update auction state *)
        SL.slice_list_to_auction_state (auctions, burrow_slices)
      (* Case: no more slices in queue, nothing to split *)
      | None -> auctions
    else
      auctions
  in

  (auctions, new_auction)

let start_liquidation_auction_if_possible
    (start_price: Common.ratio) (auctions: liquidation_auctions): liquidation_auctions =
  match auctions.current_auction with
  | Some _ -> auctions
  | None ->
    let queued_amount = avl_tok auctions.avl_storage auctions.queued_slices in
    let split_threshold =
      (* split_threshold = max (max_lot_size, FLOOR(queued_amount * min_lot_auction_queue_fraction)) *)
      let { num = num_qf; den = den_qf; } = Constants.min_lot_auction_queue_fraction in
      tok_max
        Constants.max_lot_size
        (Tok.tok_of_fraction_floor
           ((Tok.tok_to_denomination_nat queued_amount) * num_qf)
           (Tok.tok_scaling_factor_int * den_qf)
        ) in
    let (auctions, new_auction) = take_with_splitting auctions split_threshold in
    if avl_is_empty (auctions.avl_storage, new_auction)
    then
      (* If the new auction is empty (effectively meaning that the auction
       * queue itself is empty) we garbage-collect it (it's not even referenced
       * in the output). *)
      let storage = avl_delete_empty_tree (auctions.avl_storage, new_auction) in
      let current_auction = (None: current_liquidation_auction option) in
      { auctions with
        avl_storage = storage;
        current_auction = current_auction;
      }
    else
      let start_value =
        let { num = num_sp; den = den_sp; } = start_price in
        kit_of_fraction_ceil
          ((tok_to_denomination_nat (avl_tok auctions.avl_storage new_auction)) * den_sp)
          (tok_scaling_factor_int * num_sp)
      in
      let current_auction =
        Some
          { contents = new_auction;
            state = Descending (start_value, (Tezos.get_now ())); } in
      let auctions = { auctions with current_auction = current_auction; } in

      auctions

(** Compute the current threshold for a bid to be accepted. For a descending
  * auction this amounts to the reserve price (which is exponentially
  * dropping). For a descending auction we should improve upon the last bid
  * a fixed factor. *)
let liquidation_auction_current_auction_minimum_bid (auction: current_liquidation_auction) : kit =
  kit_max (kit_of_denomination (1n))
    (match auction.state with
     | Descending (start_value, start_time) ->
       let auction_decay_rate = FixedPoint.fixedpoint_of_ratio_ceil Constants.auction_decay_rate in
       let now = Tezos.get_now () in
       let time_passed = now - start_time in
       let seconds_passed = abs time_passed in

       let decay = FixedPoint.fixedpoint_pow (FixedPoint.fixedpoint_sub FixedPoint.fixedpoint_one auction_decay_rate) seconds_passed in
       kit_scale start_value decay
     | Ascending (leading_bid, _timestamp, _level) ->
       let bid_improvement_factor = FixedPoint.fixedpoint_of_ratio_floor Constants.bid_improvement_factor in
       Kit.kit_scale leading_bid.kit (FixedPoint.fixedpoint_add FixedPoint.fixedpoint_one bid_improvement_factor))

(** Check if an auction is complete. A descending auction declines
  * exponentially over time, so it is effectively never complete (George: I
  * guess when it reaches zero it is, but I'd expect someone to buy before
  * that?). If the auction is ascending, then every bid adds the longer of 20
  * minutes or 20 blocks to the time before the auction expires. *)
let is_liquidation_auction_complete
    (auction_state: liquidation_auction_state) : bid option =
  match auction_state with
  | Descending _ ->
    (None: bid option)
  | Ascending (b, t, h) ->
    if ((Tezos.get_now ()) - t)
        >
        Constants.max_bid_interval_in_seconds
    && ((Tezos.get_level ()) - h)
        >
        (int Constants.max_bid_interval_in_blocks)
    then Some b
    else (None: bid option)

let complete_liquidation_auction_if_possible
    (auctions: liquidation_auctions): liquidation_auctions =
  let auctions = match auctions.current_auction with
    | None -> auctions
    | Some curr -> begin
        match is_liquidation_auction_complete curr.state with
        | None -> auctions
        | Some winning_bid ->
          let (storage, completed_auctions) = match auctions.completed_auctions with
            | None ->
              let outcome =
                { winning_bid = winning_bid;
                  sold_tok=avl_tok auctions.avl_storage curr.contents;
                  younger_auction=(None: liquidation_auction_id option);
                  older_auction=(None: liquidation_auction_id option);
                } in
              let storage =
                avl_modify_root_data
                  (auctions.avl_storage,
                   curr.contents,
                   (fun (_prev: auction_outcome option) ->
                     Some outcome)) in
              (storage, {youngest=curr.contents; oldest=curr.contents})
            | Some params ->
              let {youngest=youngest; oldest=oldest} = params in
              let outcome =
                { winning_bid = winning_bid;
                  sold_tok=avl_tok auctions.avl_storage curr.contents;
                  younger_auction=Some youngest;
                  older_auction=(None: liquidation_auction_id option);
                } in
              let storage =
                avl_modify_root_data
                  (auctions.avl_storage,
                   curr.contents,
                   (fun (_prev: auction_outcome option) ->
                     Some outcome)) in
              let storage =
                avl_modify_root_data
                  (storage,
                   youngest,
                   (fun (prev: auction_outcome option) ->
                     match prev with
                     | None -> (failwith internalError_CompletedAuctionWithoutOutcome : auction_outcome option)
                     | Some xs -> Some ({xs with younger_auction=Some curr.contents})
                  )) in
              (storage, {youngest=curr.contents; oldest=oldest; }) in
          { auctions with
            avl_storage = storage;
            current_auction=(None: current_liquidation_auction option);
            completed_auctions=Some completed_auctions;
          }
      end
  in

  auctions

(** Place a bid in the current auction. Fail if the bid is too low (must be at
  * least as much as the liquidation_auction_current_auction_minimum_bid. If
  * bid placement is successful return the old winning bid, so that we can
  * credit their kit back to their account. *)
let liquidation_auction_place_bid (auction: current_liquidation_auction) (bid: bid) : (current_liquidation_auction * (bid option)) =
  if geq_kit_kit bid.kit (liquidation_auction_current_auction_minimum_bid auction)
  then
    begin
      let now = Tezos.get_now () in
      let level = Tezos.get_level () in
      let updated_auction = { auction with state = Ascending (bid, now, level); } in

      match auction.state with
      (* For descending auctions we don't have to return kit to anyone. *)
      | Descending _ ->
        (updated_auction, (None: bid option))
      (* For ascending auctions we have to credit the last winning bid to their owner. *)
      | Ascending (leading_bid, _timestamp, _level) ->
        (updated_auction, Some leading_bid)
    end
  else (failwith error_BidTooLow : current_liquidation_auction * (bid option))

let liquidation_auction_get_current_auction (auctions: liquidation_auctions) : current_liquidation_auction =
  match auctions.current_auction with
  | None -> (failwith error_NoOpenAuction : current_liquidation_auction)
  | Some curr -> curr

(* removes the slice from liquidation_auctions, fixing up the necessary pointers.
 * returns the contents of the removed slice, the tree root the slice belonged to, and the updated auctions
*)
let pop_slice (auctions, leaf_ptr: liquidation_auctions * leaf_ptr): liquidation_slice_contents * avl_ptr * liquidation_auctions =
  let element, burrow_slices = SL.slice_list_from_leaf_ptr (auctions, leaf_ptr) in
  let auctions, burrow_slices, root_ptr, contents = SL.slice_list_remove (burrow_slices, auctions, element) in
  let auctions = SL.slice_list_to_auction_state (auctions, burrow_slices) in

  ( contents
  , root_ptr
  , auctions
  )

let liquidation_auctions_cancel_slice (auctions: liquidation_auctions) (leaf_ptr: leaf_ptr) : liquidation_slice_contents * liquidation_auctions =

  let (contents, root, auctions) = pop_slice (auctions, leaf_ptr) in

  (* If the leaf doesn't belong to the queue, no need to cancel it. *)
  if ptr_of_avl_ptr root <> ptr_of_avl_ptr auctions.queued_slices
  (* FIXME: I (Dorran) think that we might be overloading the term 'unwarranted' here? *)
  then (failwith error_UnwarrantedCancellation : liquidation_slice_contents * liquidation_auctions)
  else (contents, auctions)

let completed_liquidation_auction_won_by_sender
    (avl_storage: mem) (auction_id: liquidation_auction_id): auction_outcome option =
  match avl_root_data (avl_storage, auction_id) with
  | Some outcome ->
    if outcome.winning_bid.address = Tezos.get_sender ()
    then Some outcome
    else (None: auction_outcome option)
  | None -> (None: auction_outcome option)

(* Removes the auction from completed lots list, while preserving the auction itself. *)
let liquidation_auction_pop_completed_auction (auctions, tree: liquidation_auctions * avl_ptr) : liquidation_auctions =

  let storage = auctions.avl_storage in

  let outcome = match avl_root_data (storage, tree) with
    | None -> (failwith internalError_PopCompletedAuctionAuctionNotCompleted : auction_outcome)
    | Some r -> r in
  let completed_auctions = match auctions.completed_auctions with
    | None -> (failwith internalError_PopCompletedAuctionNoCompletedAuction : completed_liquidation_auctions)
    | Some r -> r in

  (* First, fixup the completed auctions if we're dropping the
   * youngest or the oldest lot. *)
  let completed_auctions =
    match outcome.younger_auction with
    | None -> begin
        match outcome.older_auction with
        | None ->


          (None: completed_liquidation_auctions option)
        | Some older ->


          Some {completed_auctions with youngest = older }
      end
    | Some younger -> begin
        match outcome.older_auction with
        | None ->


          Some {completed_auctions with oldest = younger }
        | Some _older ->


          Some completed_auctions
      end in

  (* Then, fixup the pointers within the list.*)
  let storage =
    match outcome.younger_auction with
    | None -> storage
    | Some younger ->
      avl_modify_root_data (storage, younger, (fun (i: auction_outcome option) ->
          let i = match i with
            | None -> (failwith internalError_PopCompletedAuctionCompletedAuctionNoOutcome : auction_outcome)
            | Some i -> i in

          Some {i with older_auction=outcome.older_auction})) in
  let storage =
    match outcome.older_auction with
    | None -> storage
    | Some older ->
      avl_modify_root_data (storage, older, (fun (i: auction_outcome option) ->
          let i = match i with
            | None -> (failwith internalError_PopCompletedAuctionCompletedAuctionNoOutcome : auction_outcome)
            | Some i -> i in

          Some {i with younger_auction=outcome.younger_auction})) in

  let storage = avl_modify_root_data (storage, tree, (fun (_: auction_outcome option) ->
      Some { outcome with
             younger_auction = (None: liquidation_auction_id option);
             older_auction = (None: liquidation_auction_id option)})) in

  let auctions =
    { auctions with
      completed_auctions = completed_auctions;
      avl_storage = storage
    } in

  auctions

let liquidation_auctions_pop_completed_slice (auctions, leaf_ptr: liquidation_auctions * leaf_ptr) : liquidation_slice_contents * auction_outcome * liquidation_auctions =

  let (contents, root, auctions) = pop_slice (auctions, leaf_ptr) in

  (* When the auction has no slices left, we pop it from the linked list
   * of lots. We do not delete the auction itself from the storage, since
   * we still want the winner to be able to claim its result. *)
  let auctions =
    if avl_is_empty (auctions.avl_storage, root)
    then liquidation_auction_pop_completed_auction (auctions, root)
    else auctions in
  let outcome =
    match avl_root_data (auctions.avl_storage, root) with
    | None -> (failwith error_NotACompletedSlice: auction_outcome)
    | Some outcome -> outcome in

  (contents, outcome, auctions)

[@inline] let liquidation_auction_claim_win (auctions: liquidation_auctions) (auction_id: liquidation_auction_id) : (tok * liquidation_auctions) =

  let sold_tok, auctions = match completed_liquidation_auction_won_by_sender auctions.avl_storage auction_id with
    | Some outcome ->
      (* A winning bid can only be claimed when all the liquidation slices
       * for that lot is cleaned. *)
      if not (avl_is_empty (auctions.avl_storage, auction_id))
      then (failwith error_NotAllSlicesClaimed : tok * liquidation_auctions)
      else (
        (* When the winner reclaims their bid, we finally remove every reference
           to the auction. This saves storage and forbids double-claiming the
           winnings. *)


        let auctions =
          { auctions with
            avl_storage = avl_delete_empty_tree (auctions.avl_storage, auction_id) } in
        (outcome.sold_tok, auctions)
      )
    | None -> (failwith error_NotAWinningBid : tok * liquidation_auctions)
  in

  sold_tok, auctions

let liquidation_auction_touch (auctions: liquidation_auctions) (price: Common.ratio) : liquidation_auctions =

  let auctions =
    (start_liquidation_auction_if_possible price
       (complete_liquidation_auction_if_possible
          auctions)) in

  auctions


let liquidation_auction_oldest_completed_liquidation_slice (auctions: liquidation_auctions) : leaf_ptr option =
  match auctions.completed_auctions with
  | None -> (None: leaf_ptr option)
  | Some completed_auctions -> begin
      match avl_peek_front (auctions.avl_storage, completed_auctions.youngest) with
      | None -> (failwith internalError_OldestCompletedSliceEmptyCompletedAuction : leaf_ptr option)
      | Some p ->
        let (leaf_ptr, _) = p in
        Some leaf_ptr
    end

let is_burrow_done_with_liquidations (auctions: liquidation_auctions) (burrow: burrow_id) =
  match Big_map.find_opt burrow auctions.burrow_slices with
  | None -> true
  | Some bs ->
    let root = avl_find_root (auctions.avl_storage, bs.oldest_slice) in
    let outcome = avl_root_data (auctions.avl_storage, root) in
    (match outcome with
     | None -> true
     | Some _ -> false)
