#include "./error.mligo"
#include "./burrowOrigination.mligo"
#import "./avl.mligo" "AVL"
#import "./common.mligo" "Common"
#import "./burrow.mligo" "Burrow"
#import "./cfmm.mligo" "CFMM"
#import "./fixedPoint.mligo" "FixedPoint"
#import "./kit.mligo" "Kit"
#import "./lqt.mligo" "Lqt"
#import "./mem.mligo" "Mem"
#import "./ctok.mligo" "Ctok"
#import "./constants.mligo" "Constants"
#import "./tok.mligo" "Tok"
#import "./parameters.mligo" "Parameters"
#import "./tokenMetadata.mligo" "Tokens"
#import "./checkerTypes.mligo" "CheckerT"
#import "./fa2Implementation.mligo" "FA2"
#import "./price.mligo" "Price"
#import "./getOracleEntrypoint.mligo" "Oracle"
#import "./liquidationAuction.mligo" "Liquidation"

(* ************************************************************************* *)
(**                               BURROWS                                    *)
(* ************************************************************************* *)

[@inline] let find_burrow (burrows: CheckerT.burrow_map) (burrow_id: CheckerT.burrow_id) : Burrow.burrow =
  match Big_map.find_opt burrow_id burrows with
  | None -> (failwith error_NonExistentBurrow : Burrow.burrow)
  | Some burrow -> burrow

(* Looks up a burrow_id from state, and checks if the resulting burrow does
 * not have any completed liquidation slices that need to be claimed before
 * any operation. *)
let ensure_burrow_has_no_unclaimed_slices (auctions: Liquidation.liquidation_auctions) (burrow_id: CheckerT.burrow_id) : unit =
  if Liquidation.is_burrow_done_with_liquidations auctions burrow_id
  then ()
  else failwith error_BurrowHasCompletedLiquidation

(* Ensure that the given pointer exists and that it points to a Root node. *)
[@inline] let ensure_valid_avl_ptr (mem: Mem.mem) (avl_ptr: Liquidation.avl_ptr) : unit =
  match Mem.mem_get_opt mem (match avl_ptr with AVLPtr r -> r) with
  | Some (Root _) -> ()
  | _ -> failwith error_InvalidAvlPtr

(* Ensure that the given pointer exists and that it points to a Leaf node. *)
[@inline] let ensure_valid_leaf_ptr (mem: Mem.mem) (leaf_ptr: Liquidation.leaf_ptr) : unit =
  match Mem.mem_get_opt mem (match leaf_ptr with LeafPtr r -> r) with
  | Some (Leaf _) -> ()
  | _ -> failwith error_InvalidLeafPtr

[@inline] let entrypoint_create_burrow (state, (burrow_no, delegate_opt, tok): CheckerT.checker * (nat * key_hash option * Tok.tok)) =
  let burrow_id = (Tezos.get_sender (), burrow_no) in
  let () = if Big_map.mem burrow_id state.burrows
    then failwith error_BurrowAlreadyExists
    else () in
  (* OP 1: Originate a new burrow *)
  let origination_op, burrow_address = originate_burrow state delegate_opt in
  (* OP 2: Transfer the collateral to the new burrow's account. *)
  let transfer =
    { from_ = Tezos.get_sender (); (* from: FA2 account of burrow owner *)
      txs = [
        { to_ = burrow_address;   (* to: FA2 account of the burrow contract *)
          token_id = Tokens.tok_token_id;
          amount = Tok.tok_to_denomination_nat tok; (* NOTE!!! The creation deposit is in the burrow too, even if we don't consider it to be collateral! *)
        }
      ];
    } in
  let transfer_op =
    Tezos.transaction
      [transfer] (0mutez)
      (CheckerT.get_transfer_collateral_fa2_entrypoint state.external_contracts) in

  let burrow = Burrow.burrow_create state.parameters burrow_address tok in
  let state = {state with burrows = Big_map.update burrow_id (Some burrow) state.burrows} in

  ([origination_op; transfer_op], state)

let entrypoint_touch_burrow
  (state, burrow_id: CheckerT.checker * CheckerT.burrow_id) : operation list * CheckerT.checker =
  let burrow = find_burrow state.burrows burrow_id in
  let burrow = Burrow.burrow_touch state.parameters burrow in
  let state = {state with burrows = Big_map.update burrow_id (Some burrow) state.burrows} in

  (([]: operation list), state)

let entrypoint_deposit_collateral
  (state, (burrow_no, tok): CheckerT.checker * (nat * Tok.tok)) : (operation list * CheckerT.checker) =
  let burrow_id = (Tezos.get_sender (), burrow_no) in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  let burrow = Burrow.burrow_deposit_collateral state.parameters tok burrow in
  let transfer =
    { from_ = Tezos.get_sender ();        (* from: FA2 account of burrow owner *)
      txs = [
        { to_ = (Burrow.burrow_address burrow); (* to: FA2 account of the burrow contract *)
          token_id = Tokens.tok_token_id;
          amount = Tok.tok_to_denomination_nat tok; (* NOTE!!! The creation deposit is in the burrow too, even if we don't consider it to be collateral! *)
        }
      ];
    } in
  let op =
    Tezos.transaction
      [transfer] (0mutez)
      (CheckerT.get_transfer_collateral_fa2_entrypoint state.external_contracts) in
  let state = {state with burrows = Big_map.update burrow_id (Some burrow) state.burrows} in

  ([op], state)

let entrypoint_mint_kit
  (state, (burrow_no, kit): CheckerT.checker * (nat * Kit.kit)) : operation list * CheckerT.checker =
  let burrow_id = (Tezos.get_sender (), burrow_no) in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  let burrow = Burrow.burrow_mint_kit state.parameters kit burrow in
  let state =
    { state with
      burrows = Big_map.update burrow_id (Some burrow) state.burrows;
      parameters = Parameters.add_outstanding_and_circulating_kit state.parameters kit;
      fa2_state = FA2.ledger_issue_kit (state.fa2_state, Tezos.get_sender (), kit);
    } in

  (([]: operation list), state)

let entrypoint_withdraw_collateral (state, (burrow_no, tok): CheckerT.checker * (nat * Tok.tok)) : operation list * CheckerT.checker =
  let burrow_id = (Tezos.get_sender (), burrow_no) in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  let burrow = Burrow.burrow_withdraw_collateral state.parameters tok burrow in
  let state = {state with burrows = Big_map.update burrow_id (Some burrow) state.burrows} in
  let op = match (Tezos.get_entrypoint_opt "%burrowTransfer" (Burrow.burrow_address burrow): (address * nat) contract option) with
    | Some c -> Tezos.transaction (Tezos.get_sender (), Tok.tok_to_denomination_nat tok) (0mutez) c
    | None -> (failwith error_GetEntrypointOptFailureBurrowTransfer : operation) in
  ([op], state)

let entrypoint_burn_kit (state, (burrow_no, kit): CheckerT.checker * (nat * Kit.kit)) : operation list * CheckerT.checker =
  let burrow_id = (Tezos.get_sender (), burrow_no) in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  let burrow, actual_burned = Burrow.burrow_burn_kit state.parameters kit burrow in
  (* Note: there should be no way to remove more kit from circulation than what
   * is already in circulation. If anyone tries to do so, it means that they
   * try to remove kit they do not own. So, either the subtraction below will
   * fail, or [ledger_withdraw_kit] below will fail first (attempt to move kit
   * that is not owned by the sender). Either way I (George) think that this
   * assertion is only useful for our unit tests. *)

  let outstanding_to_remove = actual_burned in
  let circulating_to_remove = actual_burned in
  let state =
    {state with
     burrows = Big_map.update burrow_id (Some burrow) state.burrows;
     parameters = Parameters.remove_outstanding_and_circulating_kit state.parameters outstanding_to_remove circulating_to_remove;
     fa2_state = FA2.ledger_withdraw_kit (state.fa2_state, Tezos.get_sender (), circulating_to_remove); (* the burrow owner keeps the rest *)
    } in

  (([]: operation list), state)

let entrypoint_activate_burrow (state, (burrow_no, tok): CheckerT.checker * (nat * Tok.tok)) : operation list * CheckerT.checker =
  let burrow_id = (Tezos.get_sender (), burrow_no) in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  let burrow = Burrow.burrow_activate state.parameters tok burrow in
  let transfer =
    { from_ = Tezos.get_sender (); (* from: FA2 account of burrow owner *)
      txs = [
        { to_ = (Burrow.burrow_address burrow);   (* to: FA2 account of the burrow contract *)
          token_id = Tokens.tok_token_id;
          amount = Tok.tok_to_denomination_nat tok; (* NOTE!!! The creation deposit is in the burrow too, even if we don't consider it to be collateral! *)
        }
      ];
    } in
  let op =
    Tezos.transaction
      [transfer] (0mutez)
      (CheckerT.get_transfer_collateral_fa2_entrypoint state.external_contracts) in
  let state = {state with burrows = Big_map.update burrow_id (Some burrow) state.burrows} in
  ([op], state)

let entrypoint_deactivate_burrow (state, (burrow_no, receiver): CheckerT.checker * (nat * address)) : (operation list * CheckerT.checker) =
  let burrow_id = (Tezos.get_sender (), burrow_no) in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  let (burrow, returned_tok) = Burrow.burrow_deactivate state.parameters burrow in
  let state = {state with burrows = Big_map.update burrow_id (Some burrow) state.burrows} in
  let op = match (Tezos.get_entrypoint_opt "%burrowTransfer" (Burrow.burrow_address burrow): (address * nat) contract option) with
    | Some c -> Tezos.transaction (receiver, Tok.tok_to_denomination_nat returned_tok) (0mutez) c (* NOTE: returned_tok inlcudes creation deposit! *)
    | None -> (failwith error_GetEntrypointOptFailureBurrowTransfer : operation) in

  ([op], state)

let entrypoint_set_burrow_delegate (state, (burrow_no, delegate_opt): CheckerT.checker * (nat * key_hash option)) : operation list * CheckerT.checker =

  let burrow_id = (Tezos.get_sender (), burrow_no) in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  let op = match (Tezos.get_entrypoint_opt "%burrowSetDelegate" (Burrow.burrow_address burrow) : key_hash option contract option) with
    | Some c -> Tezos.transaction delegate_opt (0mutez) c
    | None -> (failwith error_GetEntrypointOptFailureBurrowSetDelegate : operation) in

  ([op], state)

[@inline] let entrypoint_mark_for_liquidation (state, burrow_id: CheckerT.checker * CheckerT.burrow_id) : (operation list * CheckerT.checker) =

  let burrow = find_burrow state.burrows burrow_id in

  let
    { liquidation_reward = liquidation_reward;
      collateral_to_auction = collateral_to_auction;
      burrow_state = burrow;
    } = match Burrow.burrow_request_liquidation state.parameters burrow with
    | None ->
      (* Note: disabling coverage for the unreported but accessed right-hand side;
       * accessibility is sufficiently marked on the pattern itself. *)
      ((failwith error_NotLiquidationCandidate (* [@coverage off] *)): Burrow.liquidation_details)
    | Some type_and_details -> let _, details = type_and_details in details
  in

  let state =
    if collateral_to_auction = Tok.tok_zero then
      (* If the slice would be empty, don't create it. *)
      { state with burrows = Big_map.update burrow_id (Some burrow) state.burrows; }
    else
      (* Otherwise do. *)
      let contents =
        { burrow = burrow_id;
          tok = collateral_to_auction;
          min_kit_for_unwarranted = Burrow.compute_min_kit_for_unwarranted state.parameters burrow collateral_to_auction;
        } in
      let (updated_liquidation_auctions, _leaf_ptr) =
        Liquidation.liquidation_auction_send_to_auction state.liquidation_auctions contents in
      { state with
        burrows = Big_map.update burrow_id (Some burrow) state.burrows;
        liquidation_auctions = updated_liquidation_auctions;
      }
  in
  let op = match (Tezos.get_entrypoint_opt "%burrowTransfer" (Burrow.burrow_address burrow): (address * nat) contract option) with
    | Some c -> Tezos.transaction (Tezos.get_sender (), Tok.tok_to_denomination_nat liquidation_reward) (0mutez) c
    | None -> (failwith error_GetEntrypointOptFailureBurrowTransfer : operation) in
  ([op], state)

(* Cancel the liquidation of a slice. *)
let entrypoint_cancel_liquidation_slice (state, leaf_ptr: CheckerT.checker * Liquidation.leaf_ptr) : (operation list * CheckerT.checker) =
  let _ = ensure_valid_leaf_ptr state.liquidation_auctions.avl_storage leaf_ptr in
  let (cancelled, auctions) = Liquidation.liquidation_auctions_cancel_slice state.liquidation_auctions leaf_ptr in
  let (burrow_owner, _) = cancelled.burrow in
  let burrow = find_burrow state.burrows cancelled.burrow in
  let burrow = Burrow.burrow_touch state.parameters burrow in
  let _ =
    if Burrow.burrow_is_cancellation_warranted state.parameters burrow cancelled.tok
    then ()
    else failwith error_UnwarrantedCancellation in
  if Tezos.get_sender () = burrow_owner then
    let burrow = Burrow.burrow_return_slice_from_auction cancelled burrow in
    let state =
      { state with
        burrows = Big_map.add cancelled.burrow burrow state.burrows;
        liquidation_auctions = auctions;
      } in

    (([]:  operation list), state)
  else
    (failwith error_AuthenticationError : operation list * CheckerT.checker)

(* Note that this function prepends the operation to the list of operations
 * given. This means that if we entrypoint_touch a list of liquidation slices,
 * the order of operations is reversed. *)
let touch_liquidation_slice
    (ops: operation list)
    (auctions: Liquidation.liquidation_auctions)
    (state_burrows: CheckerT.burrow_map)
    (state_parameters: Parameters.parameters)
    (state_fa2_state: FA2.fa2_state)
    (leaf_ptr: Liquidation.leaf_ptr)
  : (operation list * Liquidation.liquidation_auctions * CheckerT.burrow_map * Parameters.parameters
  * FA2.fa2_state) =

  let _ = ensure_valid_leaf_ptr auctions.avl_storage leaf_ptr in

  let slice, outcome, auctions = Liquidation.liquidation_auctions_pop_completed_slice auctions leaf_ptr in

  (* How much kit should be given to the burrow and how much should be burned.
   * Note that we treat each slice in a lot separately, so Sum(kit_to_repay_i +
   * kit_to_burn_i)_{1..n} might not add up to outcome.winning_bid.kit, due to
   * truncation. That could be a problem; the extra kit, no matter how small,
   * must be dealt with (e.g. be removed from the circulating kit).
   *
   *   kit_corresponding_to_slice =
   *     FLOOR (outcome.winning_bid.kit * (leaf.tez / outcome.sold_tez))
   *   penalty =
   *     CEIL (kit_corresponding_to_slice * penalty_percentage)  , if (corresponding_kit < leaf.min_kit_for_unwarranted)
   *     zero                                                    , otherwise
   *   kit_to_repay = kit_corresponding_to_slice - penalty
  *)
  let slice_kit, kit_to_repay, kit_to_burn =
    let corresponding_kit =
      Kit.kit_of_fraction_floor
        ((Tok.tok_to_denomination_int slice.tok) * (Kit.kit_to_denomination_nat outcome.winning_bid.kit))
        ((Tok.tok_to_denomination_nat outcome.sold_tok) * Kit.kit_scaling_factor_int)
    in
    let penalty =
      let { num = num_lp; den = den_lp; } = Constants.liquidation_penalty in
      let liquidation_was_warranted =
        match slice.min_kit_for_unwarranted with
        | None -> true
        | Some min_kit_for_unwarranted -> corresponding_kit < min_kit_for_unwarranted in
      if liquidation_was_warranted then
        Kit.kit_of_fraction_ceil
          ((Kit.kit_to_denomination_nat corresponding_kit) * num_lp)
          (Kit.kit_scaling_factor_int * den_lp)
      else
        Kit.kit_zero
    in
    (corresponding_kit, Kit.kit_sub corresponding_kit penalty, penalty)
  in

  let burrow_owner, _burrow_id = slice.burrow in
  let burrow = find_burrow state_burrows slice.burrow in
  let burrow, repaid_kit, excess_kit = Burrow.burrow_return_kit_from_auction slice kit_to_repay burrow in
  let state_burrows = Big_map.update slice.burrow (Some burrow) state_burrows in

  (* So, there are a few amounts of kit here:
   * - kit_to_repay: this is the part of the bid to be repaid to the burrow. It
   *   can be seen as consisting of two parts:
   *   + repaid_kit: this is the part of the bid that is repaid to the burrow.
   *     It is subtracted from the burrow's outstanding kit (and, consequently,
   *     from the total outstanding kit), and the circulating kit.
   *   + excess_kit: if all the outstanding kit for the burrow has been
   *     completely repaid via the repaid_kit, this part of the bid is simply
   *     returned to the burrow owner.
   * - kit_to_burn: this is the part of the bid to be destroyed. This means
   *   that we have to remove it from circulation, but not from the burrow's
   *   (and, consequently, the total) outstanding kit. This is zero if the
   *   liquidation is deemed unwarranted. *)



  let outstanding_to_remove = repaid_kit in (* excess_kit is returned to the owner and kit_to_burn is not included *)
  let circulating_to_remove = repaid_kit + kit_to_burn in (* excess_kit remains in circulation *)

  let state_parameters =
    Parameters.remove_outstanding_and_circulating_kit
      state_parameters
      outstanding_to_remove
      circulating_to_remove in

  let new_state_fa2_state =
    let state_fa2_state = FA2.ledger_withdraw_kit (state_fa2_state, Tezos.get_self_address (), slice_kit) in
    let state_fa2_state = FA2.ledger_issue_kit (state_fa2_state, burrow_owner, excess_kit) in
    state_fa2_state in

  let state_fa2_state = new_state_fa2_state in

  (* Signal the burrow to send the collateral to checker. *)
  let op = match (Tezos.get_entrypoint_opt "%burrowTransfer" (Burrow.burrow_address burrow): (address * nat) contract option) with
    | Some c -> Tezos.transaction (Tezos.get_self_address (), Tok.tok_to_denomination_nat slice.tok) (0mutez) c
    | None -> (failwith error_GetEntrypointOptFailureBurrowTransfer : operation) in
  ((op :: ops), auctions, state_burrows, state_parameters, state_fa2_state)

(* NOTE: The list of operations returned is in reverse order (with respect to
 * the order the input slices were processed in). However, since the operations
 * computed are independent from each other, this needs not be a problem. *)
let rec touch_liquidation_slices_rec
    (ops, state_liquidation_auctions, state_burrows, state_parameters, state_fa2_state, slices
    : operation list * Liquidation.liquidation_auctions * CheckerT.burrow_map * Parameters.parameters * FA2.fa2_state * Liquidation.leaf_ptr list)
  : (operation list * Liquidation.liquidation_auctions * CheckerT.burrow_map * Parameters.parameters * FA2.fa2_state) =
  match slices with
  | [] -> (ops, state_liquidation_auctions, state_burrows, state_parameters, state_fa2_state)
  | x::xs ->
    let ops, state_liquidation_auctions, state_burrows, state_parameters, state_fa2_state =
      touch_liquidation_slice ops state_liquidation_auctions state_burrows state_parameters state_fa2_state x in
    touch_liquidation_slices_rec
      (ops, state_liquidation_auctions, state_burrows, state_parameters, state_fa2_state, xs)

(* NOTE: The list of operations returned is in reverse order (with respect to
 * the order the input slices were processed in). However, since the operations
 * computed are independent from each other, this needs not be a problem. *)
[@inline] let entrypoint_touch_liquidation_slices (state, slices: CheckerT.checker * Liquidation.leaf_ptr list): (operation list * CheckerT.checker) =

  (* NOTE: the order of the operations is reversed here (wrt to the order of
   * the slices), but hopefully we don't care in this instance about this. *)
  let
    { burrows = state_burrows;
      cfmm = state_cfmm;
      parameters = state_parameters;
      liquidation_auctions = state_liquidation_auctions;
      last_index = state_last_index;
      last_ctez_in_tez = state_last_ctez_in_tez;
      fa2_state = state_fa2_state;
      external_contracts = state_external_contracts;
    } = state in

  let ops, state_liquidation_auctions, state_burrows, state_parameters, state_fa2_state =
    touch_liquidation_slices_rec (([]: operation list), state_liquidation_auctions, state_burrows, state_parameters, state_fa2_state, slices) in

  let state =
    { burrows = state_burrows;
      cfmm = state_cfmm;
      parameters = state_parameters;
      liquidation_auctions = state_liquidation_auctions;
      last_index = state_last_index;
      last_ctez_in_tez = state_last_ctez_in_tez;
      fa2_state = state_fa2_state;
      external_contracts = state_external_contracts;
    } in

  (ops, state)

(* ************************************************************************* *)
(**                                 CFMM                                     *)
(* ************************************************************************* *)

let entrypoint_buy_kit (state, p: CheckerT.checker * (Ctok.ctok * Kit.kit * timestamp)) : operation list * CheckerT.checker =

  let ctok, min_kit_expected, deadline = p in
  let (kit_tokens, updated_cfmm) = CFMM.cfmm_buy_kit state.cfmm state.parameters.target ctok min_kit_expected deadline in

  let transfer =
    { from_ = Tezos.get_sender ();
      txs = [
        { to_ = Tezos.get_self_address ();
          token_id = Tokens.ctok_token_id;
          amount = Ctok.ctok_to_denomination_nat ctok;
        }
      ];
    } in
  let op =
    Tezos.transaction
      [transfer] (0mutez)
      (CheckerT.get_transfer_ctok_fa2_entrypoint state.external_contracts) in

  let state_fa2_state =
    let state_fa2_state = state.fa2_state in
    let state_fa2_state = FA2.ledger_withdraw_kit (state_fa2_state, Tezos.get_self_address (), kit_tokens) in
    let state_fa2_state = FA2.ledger_issue_kit (state_fa2_state, Tezos.get_sender (), kit_tokens) in
    state_fa2_state in




  let state =
    { state with
      cfmm = updated_cfmm;
      (* when sending/receiving kit to/from uniswap, we destroy/create it. an
       * alternative would be to instead transfer them to/from the CheckerT.checker's
       * account via an FA2 transfer call. *)
      fa2_state = state_fa2_state;
    } in



  ([op], state)

let entrypoint_sell_kit (state, p: CheckerT.checker * (Kit.kit * Ctok.ctok * timestamp)) : operation list * CheckerT.checker =

  let kit, min_ctok_expected, deadline = p in
  let (ctok, updated_cfmm) = CFMM.cfmm_sell_kit state.cfmm state.parameters.target kit min_ctok_expected deadline in

  let transfer =
    { from_ = Tezos.get_self_address ();
      txs = [
        { to_ = Tezos.get_sender ();
          token_id = Tokens.ctok_token_id;
          amount = Ctok.ctok_to_denomination_nat ctok;
        }
      ];
    } in
  let op =
    Tezos.transaction
      [transfer] (0mutez)
      (CheckerT.get_transfer_ctok_fa2_entrypoint state.external_contracts) in

  let state_fa2_state =
    let state_fa2_state = state.fa2_state in
    let state_fa2_state = FA2.ledger_withdraw_kit (state_fa2_state, Tezos.get_sender (), kit) in
    let state_fa2_state = FA2.ledger_issue_kit (state_fa2_state, Tezos.get_self_address (), kit) in
    state_fa2_state in




  let state =
    { state with
      cfmm = updated_cfmm;
      fa2_state = state_fa2_state;
    } in


  ([op], state)

let entrypoint_add_liquidity (state, p: CheckerT.checker * (Ctok.ctok * Kit.kit * Lqt.lqt * timestamp)) : operation list * CheckerT.checker =

  let ctok_deposited, max_kit_deposited, min_lqt_minted, deadline = p in
  let (lqt_tokens, kit_tokens, updated_cfmm) =
    CFMM.cfmm_add_liquidity state.cfmm ctok_deposited max_kit_deposited min_lqt_minted deadline in

  let transfer =
    { from_ = Tezos.get_sender ();
      txs = [
        { to_ = Tezos.get_self_address ();
          token_id = Tokens.ctok_token_id;
          amount = Ctok.ctok_to_denomination_nat ctok_deposited;
        }
      ];
    } in
  let op =
    Tezos.transaction
      [transfer] (0mutez)
      (CheckerT.get_transfer_ctok_fa2_entrypoint state.external_contracts) in

  let deposited_kit = Kit.kit_sub max_kit_deposited kit_tokens in

  let state_fa2_state =
    let state_fa2_state = state.fa2_state in
    let state_fa2_state = FA2.ledger_withdraw_kit (state_fa2_state, Tezos.get_sender (), deposited_kit) in
    let state_fa2_state = FA2.ledger_issue_kit (state_fa2_state, Tezos.get_self_address (), deposited_kit) in
    let state_fa2_state = FA2.ledger_issue_lqt (state_fa2_state, Tezos.get_sender (), lqt_tokens) in (* create *)
    state_fa2_state in
  let state =
    { state with
      cfmm = updated_cfmm;
      fa2_state = state_fa2_state;
    } in
  ([op], state)

let entrypoint_remove_liquidity (state, p: CheckerT.checker * (Lqt.lqt * Ctok.ctok * Kit.kit * timestamp)) : operation list * CheckerT.checker =

  let lqt_burned, min_ctok_withdrawn, min_kit_withdrawn, deadline = p in
  let (ctok, kit_tokens, updated_cfmm) =
    CFMM.cfmm_remove_liquidity state.cfmm lqt_burned min_ctok_withdrawn min_kit_withdrawn deadline in

  let transfer =
    { from_ = Tezos.get_self_address ();
      txs = [
        { to_ = Tezos.get_sender ();
          token_id = Tokens.ctok_token_id;
          amount = Ctok.ctok_to_denomination_nat ctok;
        }
      ];
    } in
  let op =
    Tezos.transaction
      [transfer] (0mutez)
      (CheckerT.get_transfer_ctok_fa2_entrypoint state.external_contracts) in

  let state_fa2_state =
    let state_fa2_state = state.fa2_state in
    let state_fa2_state = FA2.ledger_withdraw_lqt (state_fa2_state, Tezos.get_sender (), lqt_burned) in (* destroy *)
    let state_fa2_state = FA2.ledger_withdraw_kit (state_fa2_state, Tezos.get_self_address (), kit_tokens) in
    let state_fa2_state = FA2.ledger_issue_kit (state_fa2_state, Tezos.get_sender (), kit_tokens) in
    state_fa2_state in
  let state =
    { state with
      cfmm = updated_cfmm;
      fa2_state = state_fa2_state;
    } in
  ([op], state)

(* ************************************************************************* *)
(**                          LIQUIDATION AUCTIONS                            *)
(* ************************************************************************* *)

let entrypoint_liquidation_auction_place_bid (state, (auction_id, kit): CheckerT.checker * (Liquidation.liquidation_auction_id * Kit.kit)) : operation list * CheckerT.checker =


  let bid = { address=(Tezos.get_sender ()); kit=kit; } in
  let current_auction = Liquidation.liquidation_auction_get_current_auction state.liquidation_auctions in
  let () = if current_auction.contents = auction_id
    then ()
    else failwith error_InvalidLiquidationAuction in

  let (new_current_auction, old_winning_bid) = Liquidation.liquidation_auction_place_bid current_auction bid in

  (* Update the fa2_state: (a) restore the old winning bid from checker to its
   * original owner (if such a bid exists), and (b) move [kit] from the
   * bidder's account to checker's account. By performing the operation in this
   * order we allow users to just increase their bid without having to have in
   * their accounts [old_bid + new_bid] kit; having [new_bid] is enough. *)
  let state_fa2_state =
    let state_fa2_state = state.fa2_state in
    (* credit the old winning bid to its owner, if necessary *)
    let state_fa2_state =
      match old_winning_bid with
      | None -> state_fa2_state (* nothing to do *)
      | Some old_winning_bid ->
        let state_fa2_state = FA2.ledger_withdraw_kit (state_fa2_state, Tezos.get_self_address (), old_winning_bid.kit) in
        let state_fa2_state = FA2.ledger_issue_kit (state_fa2_state, old_winning_bid.address, old_winning_bid.kit) in
        state_fa2_state in


    (* credit the new winning bid to checker *)
    let state_fa2_state = FA2.ledger_withdraw_kit (state_fa2_state, Tezos.get_sender (), kit) in
    let state_fa2_state = FA2.ledger_issue_kit (state_fa2_state, Tezos.get_self_address (), kit) in

    state_fa2_state in

  let state =
    { state with
      liquidation_auctions=
        { state.liquidation_auctions with
          current_auction = Some new_current_auction;
        };
      fa2_state = state_fa2_state;
    } in



  (([]: operation list), state)

let entrypoint_liquidation_auction_claim_win (state, auction_id: CheckerT.checker * Liquidation.liquidation_auction_id) : (operation list * CheckerT.checker) =

  let _ = ensure_valid_avl_ptr state.liquidation_auctions.avl_storage auction_id in
  let (tok, liquidation_auctions) = Liquidation.liquidation_auction_claim_win state.liquidation_auctions auction_id in
  let transfer =
    { from_ = Tezos.get_self_address (); (* from: FA2 account of the checker contract *)
      txs = [
        { to_ = Tezos.get_sender ();     (* to: FA2 account of the auction winner *)
          token_id = Tokens.tok_token_id;
          amount = Tok.tok_to_denomination_nat tok;
        }
      ];
    } in
  let op =
    Tezos.transaction
      [transfer] (0mutez)
      (CheckerT.get_transfer_collateral_fa2_entrypoint state.external_contracts) in

  let state = {state with liquidation_auctions = liquidation_auctions } in


  ([op], state)

(* ************************************************************************* *)
(**                              TOUCHING                                    *)
(* ************************************************************************* *)

(** Calculate how much is right now the reward for touching the main checker
  * contract. We use a bracketed calculation, where for the first
  * touch_reward_low_bracket seconds the reward increases by touch_low_reward
  * per second, and after that by touch_high_reward per second. *)
let calculate_touch_reward (last_touched: timestamp) : Kit.kit =
  let duration_in_seconds = (Tezos.get_now ()) - last_touched in
  let low_duration = Common.min_int duration_in_seconds Constants.touch_reward_low_bracket in
  let high_duration =
    Common.max_int
      0
      (duration_in_seconds - Constants.touch_reward_low_bracket) in

  (* reward = FLOOR (low_duration * touch_low_reward + high_duration * touch_high_reward) *)
  let { num = num_tlr; den = den_tlr; } = Constants.touch_low_reward in
  let { num = num_thr; den = den_thr; } = Constants.touch_high_reward in
  Kit.kit_of_fraction_floor
    (
       (low_duration * (num_tlr * den_thr))
       +
       (high_duration * (num_thr * den_tlr))
    )
    (den_tlr * den_thr)

(* Note that the list of operations returned is in reverse order (with respect
 * to the order the input slices were processed in). However, since the
 * operations computed are independent from each other, this needs not be a
 * problem. *)
let rec touch_oldest
    (ops, state_liquidation_auctions, state_burrows, state_parameters, state_fa2_state, maximum
                                                                                        : operation list * Liquidation.liquidation_auctions * CheckerT.burrow_map * Parameters.parameters * FA2.fa2_state * nat)
  : (operation list * Liquidation.liquidation_auctions * CheckerT.burrow_map * Parameters.parameters * FA2.fa2_state) =
  match is_nat (maximum - 1n) with
  | None -> (ops, state_liquidation_auctions, state_burrows, state_parameters, state_fa2_state)
  | Some maximum ->
    begin
      match Liquidation.liquidation_auction_oldest_completed_liquidation_slice state_liquidation_auctions with
      | None -> (ops, state_liquidation_auctions, state_burrows, state_parameters, state_fa2_state)
      | Some leaf ->
        let ops, state_liquidation_auctions, state_burrows, state_parameters, state_fa2_state =
          touch_liquidation_slice ops state_liquidation_auctions state_burrows state_parameters state_fa2_state leaf in
        touch_oldest
          ( ops,
            state_liquidation_auctions,
            state_burrows,
            state_parameters,
            state_fa2_state,
            maximum
          )
    end

(* Note that the list of operations returned is in reverse order (with respect to
 * the order in which the things are expected to happen). However, all inputs
 * to those operations are computed in the correct order, and, with two
 * exceptions (1. setting the delegate, and 2. call/callback to the oract), all
 * of the operations are outwards calls, to other contracts (no callbacks). It
 * should be safe to leave the order of the transaction reversed. *)
[@inline] let touch_with_index (state: CheckerT.checker) (index: FixedPoint.fixedpoint) : (operation list * CheckerT.checker) =

  let
    { burrows = state_burrows;
      cfmm = state_cfmm;
      parameters = state_parameters;
      liquidation_auctions = state_liquidation_auctions;
      last_index = _;
      last_ctez_in_tez = state_last_ctez_in_tez;
      fa2_state = state_fa2_state;
      external_contracts = state_external_contracts;
    } = state in

  if state_parameters.last_touched = Tezos.get_now () then
    (* Do nothing if up-to-date (idempotence) *)
    (([]: operation list), state)
  else
    (* TODO: What is the right order in which to do things here? *)

    (* 1: Get the price from the oracle. Note that we do not check on the
     * liveness of the oracle. *)
    let new_index =
      let (num, den) = Oracle.get_oracle_price state_external_contracts in
      FixedPoint.fixedpoint_of_ratio_floor (Common.make_ratio (int num) (int den))
    in

    (* 2: Mint some kit out of thin air to reward the contract toucher, and
     * update the circulating kit accordingly.*)
    let reward = calculate_touch_reward state_parameters.last_touched in
    let state_parameters = Parameters.add_circulating_kit state_parameters reward in
    let state_fa2_state = FA2.ledger_issue_kit (state_fa2_state, Tezos.get_sender (), reward) in

    (* 3: Update the system parameters and add accrued burrowing fees to the
     * cfmm sub-contract. *)
    let kit_in_tok_in_prev_block, ops = Price.calculate_kit_in_tok state_cfmm state_last_ctez_in_tez state_external_contracts in
    let total_accrual_to_cfmm, state_parameters = Parameters.parameters_touch new_index kit_in_tok_in_prev_block state_parameters in
    (* Note: state_parameters.circulating kit here already includes the accrual to the CFMM. *)
    let state_cfmm = CFMM.cfmm_add_accrued_kit state_cfmm total_accrual_to_cfmm in
    let state_fa2_state = FA2.ledger_issue_kit (state_fa2_state, Tezos.get_self_address (), total_accrual_to_cfmm) in

    (* 4: Update auction-related info (e.g. start a new auction). Note that we
     * always start auctions using the current liquidation price. We could also
     * have calculated the price right now directly using the oracle feed as
     * (tz_t * q_t), or use the current minting price, but using the
     * liquidation price is the safest option. *)
    let state_liquidation_auctions =
      Liquidation.liquidation_auction_touch state_liquidation_auctions (Parameters.liquidation_price state_parameters) in

    (* 5: Touch oldest liquidation slices *)
    (* TODO: Touch only runs at most once per block. But it might be beneficial
     * to run this step without that restriction. *)

    (* TODO: Figure out how many slices we can process per checker entrypoint_touch.*)
    let ops, state_liquidation_auctions, state_burrows, state_parameters, state_fa2_state =
      touch_oldest (ops, state_liquidation_auctions, state_burrows, state_parameters, state_fa2_state, Constants.number_of_slices_to_process) in

    let state =
      { burrows = state_burrows;
        cfmm = state_cfmm;
        parameters = state_parameters;
        liquidation_auctions = state_liquidation_auctions;
        last_index = Some new_index;
        last_ctez_in_tez = state_last_ctez_in_tez;
        fa2_state = state_fa2_state;
        external_contracts = state_external_contracts;
      } in
    (ops, state)

let entrypoint_touch (state, _: CheckerT.checker * unit) : (operation list * CheckerT.checker) =
  let index = match state.last_index with
    | None -> state.parameters.index (* use the old one *)
    | Some i -> i in (* tez/chf (or chf in tez) *)
  touch_with_index state index

(* ************************************************************************* *)
(**                               ORACLE                                     *)
(* ************************************************************************* *)

(* Previous Checker was using a CPS view to receive the price. This entrypoint should
 * not be used anymore under normal conditions, but we keep it as a failsafe for now. *)
let entrypoint_receive_price (state, oracle_price: CheckerT.checker * (nat * nat)) : (operation list * CheckerT.checker) =

  if Tezos.get_sender () <> state.external_contracts.oracle then
    (failwith error_UnauthorisedCaller : operation list * CheckerT.checker)
  else
    (* NOTE: By storing the price as a fixedpoint we lose some precision here.
     * However, keeping the original fraction here could become much more
     * costly in other places (in the calculation of the index and the
     * protected index, for example). Another alternative (with more-or-less
     * the same effect) would be to keep the last_index field as a fraction and
     * convert it in the call to touch. *)
    let price =
      let (num, den) = oracle_price in
      FixedPoint.fixedpoint_of_ratio_floor (Common.make_ratio (int num) (int den)) in
    (([]: operation list), {state with last_index = Some price})

let entrypoint_receive_ctez_marginal_price (state, price: CheckerT.checker * (nat * nat)) : (operation list * CheckerT.checker) =

  if Tezos.get_sender () <> state.external_contracts.ctez_cfmm then
    (failwith error_UnauthorisedCaller : operation list * CheckerT.checker)
  else
    let num_tez, den_ctez = price in
    let state_last_ctez_in_tez = { num = int num_tez; den = int den_ctez; } in
    (([]: operation list), {state with last_ctez_in_tez = Some state_last_ctez_in_tez})

(* ************************************************************************* *)
(**                               FA2                                        *)
(* ************************************************************************* *)

let strict_entrypoint_transfer (state, xs: CheckerT.checker * FA2.fa2_transfer list) : (operation list * CheckerT.checker) =

  let state = { state with fa2_state = FA2.fa2_run_transfer (state.fa2_state, xs) } in

  (([]: operation list), state)

[@inline] let strict_entrypoint_balance_of (state, param: CheckerT.checker * FA2.fa2_balance_of_param) : (operation list * CheckerT.checker) =

  let { requests = requests; callback = callback; } = param in
  let response = FA2.fa2_run_balance_of (state.fa2_state, requests) in
  let op = Tezos.transaction response (0mutez) callback in

  ([op], state)

let entrypoint_update_operators (state, xs: CheckerT.checker * FA2.fa2_update_operator list) : (operation list * CheckerT.checker) =

  let state = { state with fa2_state = FA2.fa2_run_update_operators (state.fa2_state, xs) } in

  (([]: operation list), state)

(* ************************************************************************* *)
(**                               VIEWS                                      *)
(* ************************************************************************* *)

let view_buy_kit_min_kit_expected (ctok, state: Ctok.ctok * CheckerT.checker) : Kit.kit =

  let (kit, _cfmm) = CFMM.cfmm_view_min_kit_expected_buy_kit state.cfmm state.parameters.target ctok in
  kit

let view_sell_kit_min_ctok_expected (kit, state: Kit.kit * CheckerT.checker) : Ctok.ctok =

  let (ctok, _cfmm) = CFMM.cfmm_view_min_ctok_expected_cfmm_sell_kit state.cfmm state.parameters.target kit in
  ctok

let view_add_liquidity_max_kit_deposited (ctok, state: Ctok.ctok * CheckerT.checker) : Kit.kit =

  let (_lqt, kit, _cfmm) = CFMM.cfmm_view_max_kit_deposited_min_lqt_minted_cfmm_add_liquidity state.cfmm ctok in
  kit

let view_add_liquidity_min_lqt_minted (ctok, state: Ctok.ctok * CheckerT.checker) : Lqt.lqt =

  let (lqt, _kit, _cfmm) = CFMM.cfmm_view_max_kit_deposited_min_lqt_minted_cfmm_add_liquidity state.cfmm ctok in
  lqt

let view_remove_liquidity_min_ctok_withdrawn (lqt, state: Lqt.lqt * CheckerT.checker) : Ctok.ctok =

  let (ctok, _kit, _cfmm) = CFMM.cfmm_view_min_ctok_withdrawn_min_kit_withdrawn_cfmm_remove_liquidity state.cfmm lqt in
  ctok

let view_remove_liquidity_min_kit_withdrawn (lqt, state: Lqt.lqt * CheckerT.checker) : Kit.kit =

  let (_ctok, kit, _cfmm) = CFMM.cfmm_view_min_ctok_withdrawn_min_kit_withdrawn_cfmm_remove_liquidity state.cfmm lqt in
  kit

let view_burrow_max_mintable_kit (burrow_id, state: CheckerT.burrow_id * CheckerT.checker) : Kit.kit =

  let burrow = find_burrow state.burrows burrow_id in
  let burrow = Burrow.burrow_touch state.parameters burrow in
  Burrow.burrow_max_mintable_kit state.parameters burrow

let view_is_burrow_overburrowed (burrow_id, state: CheckerT.burrow_id * CheckerT.checker) : bool =

  let burrow = find_burrow state.burrows burrow_id in
  let burrow = Burrow.burrow_touch state.parameters burrow in
  Burrow.burrow_is_overburrowed state.parameters burrow

let view_is_burrow_liquidatable (burrow_id, state: CheckerT.burrow_id * CheckerT.checker) : bool =

  let burrow = find_burrow state.burrows burrow_id in
  let burrow = Burrow.burrow_touch state.parameters burrow in
  Burrow.burrow_is_liquidatable state.parameters burrow

let view_current_liquidation_auction_details ((), state: unit * CheckerT.checker) : CheckerT.view_current_liquidation_auction_details_result =

  let auction = Liquidation.liquidation_auction_get_current_auction state.liquidation_auctions in
  let current_bid, blocks, seconds = match auction.state with
    | Descending _ -> (None: (Liquidation.bid option)), (None: (int option)), (None: (int option))
    | Ascending (current_bid, bid_time_seconds, bid_level) ->
      let level = Tezos.get_level () in
      let now = Tezos.get_now () in
      (
        Some current_bid,
        Some ((bid_level + Constants.max_bid_interval_in_blocks) - level),
        Some ((bid_time_seconds + Constants.max_bid_interval_in_seconds) - now)
      )
  in
  {
    auction_id = auction.contents;
    collateral = AVL.avl_tok state.liquidation_auctions.avl_storage auction.contents;
    minimum_bid = Liquidation.liquidation_auction_current_auction_minimum_bid auction;
    current_bid = current_bid;
    remaining_blocks = blocks;
    remaining_seconds = seconds;
  }

(* ************************************************************************* *)
(**                            FA2_VIEWS                                     *)
(* ************************************************************************* *)

let view_get_balance ((owner, token_id), state: (address * FA2.fa2_token_id) * CheckerT.checker) : nat =

  FA2.fa2_get_balance (state.fa2_state, owner, token_id)

let view_total_supply (token_id, state: FA2.fa2_token_id * CheckerT.checker) : nat =

  if token_id = Tokens.kit_token_id then
    Kit.kit_to_denomination_nat state.parameters.circulating_kit
  else if token_id = Tokens.lqt_token_id then
    Lqt.lqt_to_denomination_nat (Lqt.lqt_sub state.cfmm.lqt (Lqt.lqt_of_denomination 1n))
  else
    failwith "FA2_TOKEN_UNDEFINED"

let view_all_tokens ((), _state: unit * CheckerT.checker) : FA2.fa2_token_id list =
  FA2.fa2_all_tokens

let view_is_operator ((owner, (operator, token_id)), state: (address * (address * FA2.fa2_token_id)) * CheckerT.checker): bool =
  FA2.fa2_is_operator (state.fa2_state, operator, owner, token_id)
