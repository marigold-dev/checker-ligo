(* ************************************************************************* *)
(**                               BURROWS                                    *)
(* ************************************************************************* *)

[@inline] let find_burrow (burrows: burrow_map) (burrow_id: burrow_id) : burrow =
  match Big_map.find_opt burrow_id burrows with
  | None -> (failwith error_NonExistentBurrow : burrow)
  | Some burrow -> burrow

(* Looks up a burrow_id from state, and checks if the resulting burrow does
 * not have any completed liquidation slices that need to be claimed before
 * any operation. *)
let ensure_burrow_has_no_unclaimed_slices (auctions: liquidation_auctions) (burrow_id: burrow_id) : unit =
  if is_burrow_done_with_liquidations auctions burrow_id
  then ()
  else failwith error_BurrowHasCompletedLiquidation

(* Ensure that the given pointer exists and that it points to a Root node. *)
[@inline] let ensure_valid_avl_ptr (mem: mem) (avl_ptr: avl_ptr) : unit =
  match mem_get_opt mem (match avl_ptr with AVLPtr r -> r) with
  | Some (Root _) -> ()
  | _ -> failwith error_InvalidAvlPtr

(* Ensure that the given pointer exists and that it points to a Leaf node. *)
[@inline] let ensure_valid_leaf_ptr (mem: mem) (leaf_ptr: leaf_ptr) : unit =
  match mem_get_opt mem (match leaf_ptr with LeafPtr r -> r) with
  | Some (Leaf _) -> ()
  | _ -> failwith error_InvalidLeafPtr

[@inline] let entrypoint_create_burrow (state, (burrow_no, delegate_opt, tok): checker * (nat * key_hash option * tok)) =

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
          token_id = tok_token_id;
          amount = tok_to_denomination_nat tok; (* NOTE!!! The creation deposit is in the burrow too, even if we don't consider it to be collateral! *)
        }
      ];
    } in
  let transfer_op =
    Tezos.transaction
      [transfer] (0mutez)
      (get_transfer_collateral_fa2_entrypoint state.external_contracts) in

  let burrow = burrow_create state.parameters burrow_address tok in
  let state = {state with burrows = Big_map.update burrow_id (Some burrow) state.burrows} in

  ([origination_op; transfer_op], state)

let entrypoint_touch_burrow (state, burrow_id: checker * burrow_id) : operation list * checker =

  let burrow = find_burrow state.burrows burrow_id in
  let burrow = burrow_touch state.parameters burrow in
  let state = {state with burrows = Big_map.update burrow_id (Some burrow) state.burrows} in

  (([]: operation list), state)

let entrypoint_deposit_collateral (state, (burrow_no, tok): checker * (nat * tok)) : (operation list * checker) =

  let burrow_id = (Tezos.get_sender (), burrow_no) in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  let burrow = burrow_deposit_collateral state.parameters tok burrow in
  let transfer =
    { from_ = Tezos.get_sender ();        (* from: FA2 account of burrow owner *)
      txs = [
        { to_ = (burrow_address burrow); (* to: FA2 account of the burrow contract *)
          token_id = tok_token_id;
          amount = tok_to_denomination_nat tok; (* NOTE!!! The creation deposit is in the burrow too, even if we don't consider it to be collateral! *)
        }
      ];
    } in
  let op =
    Tezos.transaction
      [transfer] (0mutez)
      (get_transfer_collateral_fa2_entrypoint state.external_contracts) in
  let state = {state with burrows = Big_map.update burrow_id (Some burrow) state.burrows} in

  ([op], state)

let entrypoint_mint_kit (state, (burrow_no, kit): checker * (nat * kit)) : operation list * checker =

  let burrow_id = (Tezos.get_sender (), burrow_no) in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  let burrow = burrow_mint_kit state.parameters kit burrow in
  let state =
    { state with
      burrows = Big_map.update burrow_id (Some burrow) state.burrows;
      parameters = add_outstanding_and_circulating_kit state.parameters kit;
      fa2_state = ledger_issue_kit (state.fa2_state, Tezos.get_sender (), kit);
    } in

  (([]: operation list), state)

let entrypoint_withdraw_collateral (state, (burrow_no, tok): checker * (nat * tok)) : operation list * checker =

  let burrow_id = (Tezos.get_sender (), burrow_no) in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  let burrow = burrow_withdraw_collateral state.parameters tok burrow in
  let state = {state with burrows = Big_map.update burrow_id (Some burrow) state.burrows} in
  let op = match (Tezos.get_entrypoint_opt "%burrowTransfer" (burrow_address burrow): (address * nat) contract option) with
    | Some c -> Tezos.transaction (Tezos.get_sender (), tok_to_denomination_nat tok) (0mutez) c
    | None -> (failwith error_GetEntrypointOptFailureBurrowTransfer : operation) in

  ([op], state)

let entrypoint_burn_kit (state, (burrow_no, kit): checker * (nat * kit)) : operation list * checker =

  let burrow_id = (Tezos.get_sender (), burrow_no) in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  let burrow, actual_burned = burrow_burn_kit state.parameters kit burrow in
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
     parameters = remove_outstanding_and_circulating_kit state.parameters outstanding_to_remove circulating_to_remove;
     fa2_state = ledger_withdraw_kit (state.fa2_state, Tezos.get_sender (), circulating_to_remove); (* the burrow owner keeps the rest *)
    } in

  (([]: operation list), state)

let entrypoint_activate_burrow (state, (burrow_no, tok): checker * (nat * tok)) : operation list * checker =

  let burrow_id = (Tezos.get_sender (), burrow_no) in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  let burrow = burrow_activate state.parameters tok burrow in
  let transfer =
    { from_ = Tezos.get_sender (); (* from: FA2 account of burrow owner *)
      txs = [
        { to_ = (burrow_address burrow);   (* to: FA2 account of the burrow contract *)
          token_id = tok_token_id;
          amount = tok_to_denomination_nat tok; (* NOTE!!! The creation deposit is in the burrow too, even if we don't consider it to be collateral! *)
        }
      ];
    } in
  let op =
    Tezos.transaction
      [transfer] (0mutez)
      (get_transfer_collateral_fa2_entrypoint state.external_contracts) in
  let state = {state with burrows = Big_map.update burrow_id (Some burrow) state.burrows} in

  ([op], state)

let entrypoint_deactivate_burrow (state, (burrow_no, receiver): checker * (nat * address)) : (operation list * checker) =

  let burrow_id = (Tezos.get_sender (), burrow_no) in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  let (burrow, returned_tok) = burrow_deactivate state.parameters burrow in
  let state = {state with burrows = Big_map.update burrow_id (Some burrow) state.burrows} in
  let op = match (Tezos.get_entrypoint_opt "%burrowTransfer" (burrow_address burrow): (address * nat) contract option) with
    | Some c -> Tezos.transaction (receiver, tok_to_denomination_nat returned_tok) (0mutez) c (* NOTE: returned_tok inlcudes creation deposit! *)
    | None -> (failwith error_GetEntrypointOptFailureBurrowTransfer : operation) in

  ([op], state)

let entrypoint_set_burrow_delegate (state, (burrow_no, delegate_opt): checker * (nat * key_hash option)) : operation list * checker =

  let burrow_id = (Tezos.get_sender (), burrow_no) in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  let op = match (Tezos.get_entrypoint_opt "%burrowSetDelegate" (burrow_address burrow) : key_hash option contract option) with
    | Some c -> Tezos.transaction delegate_opt (0mutez) c
    | None -> (failwith error_GetEntrypointOptFailureBurrowSetDelegate : operation) in

  ([op], state)

[@inline] let entrypoint_mark_for_liquidation (state, burrow_id: checker * burrow_id) : (operation list * checker) =

  let burrow = find_burrow state.burrows burrow_id in

  let
    { liquidation_reward = liquidation_reward;
      collateral_to_auction = collateral_to_auction;
      burrow_state = burrow;
    } = match burrow_request_liquidation state.parameters burrow with
    | None ->
      (* Note: disabling coverage for the unreported but accessed right-hand side;
       * accessibility is sufficiently marked on the pattern itself. *)
      ((failwith error_NotLiquidationCandidate (* [@coverage off] *)): liquidation_details)
    | Some type_and_details -> let _, details = type_and_details in details
  in

  let state =
    if eq_tok_tok collateral_to_auction tok_zero then
      (* If the slice would be empty, don't create it. *)
      { state with burrows = Big_map.update burrow_id (Some burrow) state.burrows; }
    else
      (* Otherwise do. *)
      let contents =
        { burrow = burrow_id;
          tok = collateral_to_auction;
          min_kit_for_unwarranted = compute_min_kit_for_unwarranted state.parameters burrow collateral_to_auction;
        } in
      let (updated_liquidation_auctions, _leaf_ptr) =
        liquidation_auction_send_to_auction state.liquidation_auctions contents in
      { state with
        burrows = Big_map.update burrow_id (Some burrow) state.burrows;
        liquidation_auctions = updated_liquidation_auctions;
      } in

  let op = match (Tezos.get_entrypoint_opt "%burrowTransfer" (burrow_address burrow): (address * nat) contract option) with
    | Some c -> Tezos.transaction (Tezos.get_sender (), tok_to_denomination_nat liquidation_reward) (0mutez) c
    | None -> (failwith error_GetEntrypointOptFailureBurrowTransfer : operation) in


  ([op], state)

(* Cancel the liquidation of a slice. *)
let entrypoint_cancel_liquidation_slice (state, leaf_ptr: checker * leaf_ptr) : (operation list * checker) =

  let _ = ensure_valid_leaf_ptr state.liquidation_auctions.avl_storage leaf_ptr in
  let (cancelled, auctions) = liquidation_auctions_cancel_slice state.liquidation_auctions leaf_ptr in
  let (burrow_owner, _) = cancelled.burrow in
  let burrow = find_burrow state.burrows cancelled.burrow in
  let burrow = burrow_touch state.parameters burrow in
  let _ =
    if burrow_is_cancellation_warranted state.parameters burrow cancelled.tok
    then ()
    else failwith error_UnwarrantedCancellation in
  if Tezos.get_sender () = burrow_owner then
    let burrow = burrow_return_slice_from_auction cancelled burrow in
    let state =
      { state with
        burrows = Big_map.add cancelled.burrow burrow state.burrows;
        liquidation_auctions = auctions;
      } in

    (([]:  operation list), state)
  else
    (failwith error_AuthenticationError : operation list * checker)

(* Note that this function prepends the operation to the list of operations
 * given. This means that if we entrypoint_touch a list of liquidation slices,
 * the order of operations is reversed. *)
let touch_liquidation_slice
    (ops: operation list)
    (auctions: liquidation_auctions)
    (state_burrows: burrow_map)
    (state_parameters: parameters)
    (state_fa2_state: fa2_state)
    (leaf_ptr: leaf_ptr)
  : (operation list * liquidation_auctions * burrow_map * parameters * fa2_state) =

  let _ = ensure_valid_leaf_ptr auctions.avl_storage leaf_ptr in

  let slice, outcome, auctions = liquidation_auctions_pop_completed_slice auctions leaf_ptr in

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
      kit_of_fraction_floor
        (mul_int_nat (tok_to_denomination_int slice.tok) (kit_to_denomination_nat outcome.winning_bid.kit))
        (mul_nat_int (tok_to_denomination_nat outcome.sold_tok) kit_scaling_factor_int)
    in
    let penalty =
      let { num = num_lp; den = den_lp; } = liquidation_penalty in
      let liquidation_was_warranted =
        match slice.min_kit_for_unwarranted with
        | None -> true
        | Some min_kit_for_unwarranted -> lt_kit_kit corresponding_kit min_kit_for_unwarranted in
      if liquidation_was_warranted then
        kit_of_fraction_ceil
          (mul_nat_int (kit_to_denomination_nat corresponding_kit) num_lp)
          (mul_int_int kit_scaling_factor_int den_lp)
      else
        kit_zero
    in
    (corresponding_kit, kit_sub corresponding_kit penalty, penalty)
  in

  let burrow_owner, _burrow_id = slice.burrow in
  let burrow = find_burrow state_burrows slice.burrow in
  let burrow, repaid_kit, excess_kit = burrow_return_kit_from_auction slice kit_to_repay burrow in
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
  let circulating_to_remove = kit_add repaid_kit kit_to_burn in (* excess_kit remains in circulation *)

  let state_parameters =
    remove_outstanding_and_circulating_kit
      state_parameters
      outstanding_to_remove
      circulating_to_remove in

  let new_state_fa2_state =
    let state_fa2_state = ledger_withdraw_kit (state_fa2_state, Tezos.get_self_address (), slice_kit) in
    let state_fa2_state = ledger_issue_kit (state_fa2_state, burrow_owner, excess_kit) in
    state_fa2_state in




  let state_fa2_state = new_state_fa2_state in

  (* Signal the burrow to send the collateral to checker. *)
  let op = match (Tezos.get_entrypoint_opt "%burrowTransfer" (burrow_address burrow): (address * nat) contract option) with
    | Some c -> Tezos.transaction (Tezos.get_self_address (), tok_to_denomination_nat slice.tok) (0mutez) c
    | None -> (failwith error_GetEntrypointOptFailureBurrowTransfer : operation) in
  ((op :: ops), auctions, state_burrows, state_parameters, state_fa2_state)

(* NOTE: The list of operations returned is in reverse order (with respect to
 * the order the input slices were processed in). However, since the operations
 * computed are independent from each other, this needs not be a problem. *)
let rec touch_liquidation_slices_rec
    (ops, state_liquidation_auctions, state_burrows, state_parameters, state_fa2_state, slices
                                                                                        : operation list * liquidation_auctions * burrow_map * parameters * fa2_state * leaf_ptr list)
  : (operation list * liquidation_auctions * burrow_map * parameters * fa2_state) =
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
[@inline] let entrypoint_touch_liquidation_slices (state, slices: checker * leaf_ptr list): (operation list * checker) =

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

let entrypoint_buy_kit (state, p: checker * (ctok * kit * timestamp)) : operation list * checker =

  let ctok, min_kit_expected, deadline = p in
  let (kit_tokens, updated_cfmm) = cfmm_buy_kit state.cfmm state.parameters.target ctok min_kit_expected deadline in

  let transfer =
    { from_ = Tezos.get_sender ();
      txs = [
        { to_ = Tezos.get_self_address ();
          token_id = ctok_token_id;
          amount = ctok_to_denomination_nat ctok;
        }
      ];
    } in
  let op =
    Tezos.transaction
      [transfer] (0mutez)
      (get_transfer_ctok_fa2_entrypoint state.external_contracts) in

  let state_fa2_state =
    let state_fa2_state = state.fa2_state in
    let state_fa2_state = ledger_withdraw_kit (state_fa2_state, Tezos.get_self_address (), kit_tokens) in
    let state_fa2_state = ledger_issue_kit (state_fa2_state, Tezos.get_sender (), kit_tokens) in
    state_fa2_state in




  let state =
    { state with
      cfmm = updated_cfmm;
      (* when sending/receiving kit to/from uniswap, we destroy/create it. an
       * alternative would be to instead transfer them to/from the checker's
       * account via an FA2 transfer call. *)
      fa2_state = state_fa2_state;
    } in



  ([op], state)

let entrypoint_sell_kit (state, p: checker * (kit * ctok * timestamp)) : operation list * checker =

  let kit, min_ctok_expected, deadline = p in
  let (ctok, updated_cfmm) = cfmm_sell_kit state.cfmm state.parameters.target kit min_ctok_expected deadline in

  let transfer =
    { from_ = Tezos.get_self_address ();
      txs = [
        { to_ = Tezos.get_sender ();
          token_id = ctok_token_id;
          amount = ctok_to_denomination_nat ctok;
        }
      ];
    } in
  let op =
    Tezos.transaction
      [transfer] (0mutez)
      (get_transfer_ctok_fa2_entrypoint state.external_contracts) in

  let state_fa2_state =
    let state_fa2_state = state.fa2_state in
    let state_fa2_state = ledger_withdraw_kit (state_fa2_state, Tezos.get_sender (), kit) in
    let state_fa2_state = ledger_issue_kit (state_fa2_state, Tezos.get_self_address (), kit) in
    state_fa2_state in




  let state =
    { state with
      cfmm = updated_cfmm;
      fa2_state = state_fa2_state;
    } in


  ([op], state)

let entrypoint_add_liquidity (state, p: checker * (ctok * kit * lqt * timestamp)) : operation list * checker =

  let ctok_deposited, max_kit_deposited, min_lqt_minted, deadline = p in
  let (lqt_tokens, kit_tokens, updated_cfmm) =
    cfmm_add_liquidity state.cfmm ctok_deposited max_kit_deposited min_lqt_minted deadline in

  let transfer =
    { from_ = Tezos.get_sender ();
      txs = [
        { to_ = Tezos.get_self_address ();
          token_id = ctok_token_id;
          amount = ctok_to_denomination_nat ctok_deposited;
        }
      ];
    } in
  let op =
    Tezos.transaction
      [transfer] (0mutez)
      (get_transfer_ctok_fa2_entrypoint state.external_contracts) in

  let deposited_kit = kit_sub max_kit_deposited kit_tokens in

  let state_fa2_state =
    let state_fa2_state = state.fa2_state in
    let state_fa2_state = ledger_withdraw_kit (state_fa2_state, Tezos.get_sender (), deposited_kit) in
    let state_fa2_state = ledger_issue_kit (state_fa2_state, Tezos.get_self_address (), deposited_kit) in
    let state_fa2_state = ledger_issue_lqt (state_fa2_state, Tezos.get_sender (), lqt_tokens) in (* create *)
    state_fa2_state in
  let state =
    { state with
      cfmm = updated_cfmm;
      fa2_state = state_fa2_state;
    } in
  ([op], state)

let entrypoint_remove_liquidity (state, p: checker * (lqt * ctok * kit * timestamp)) : operation list * checker =

  let lqt_burned, min_ctok_withdrawn, min_kit_withdrawn, deadline = p in
  let (ctok, kit_tokens, updated_cfmm) =
    cfmm_remove_liquidity state.cfmm lqt_burned min_ctok_withdrawn min_kit_withdrawn deadline in

  let transfer =
    { from_ = Tezos.get_self_address ();
      txs = [
        { to_ = Tezos.get_sender ();
          token_id = ctok_token_id;
          amount = ctok_to_denomination_nat ctok;
        }
      ];
    } in
  let op =
    Tezos.transaction
      [transfer] (0mutez)
      (get_transfer_ctok_fa2_entrypoint state.external_contracts) in

  let state_fa2_state =
    let state_fa2_state = state.fa2_state in
    let state_fa2_state = ledger_withdraw_lqt (state_fa2_state, Tezos.get_sender (), lqt_burned) in (* destroy *)
    let state_fa2_state = ledger_withdraw_kit (state_fa2_state, Tezos.get_self_address (), kit_tokens) in
    let state_fa2_state = ledger_issue_kit (state_fa2_state, Tezos.get_sender (), kit_tokens) in
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

let entrypoint_liquidation_auction_place_bid (state, (auction_id, kit): checker * (liquidation_auction_id * kit)) : operation list * checker =


  let bid = { address=(Tezos.get_sender ()); kit=kit; } in
  let current_auction = liquidation_auction_get_current_auction state.liquidation_auctions in
  let () = if current_auction.contents = auction_id
    then ()
    else failwith error_InvalidLiquidationAuction in

  let (new_current_auction, old_winning_bid) = liquidation_auction_place_bid current_auction bid in

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
        let state_fa2_state = ledger_withdraw_kit (state_fa2_state, Tezos.get_self_address (), old_winning_bid.kit) in
        let state_fa2_state = ledger_issue_kit (state_fa2_state, old_winning_bid.address, old_winning_bid.kit) in
        state_fa2_state in


    (* credit the new winning bid to checker *)
    let state_fa2_state = ledger_withdraw_kit (state_fa2_state, Tezos.get_sender (), kit) in
    let state_fa2_state = ledger_issue_kit (state_fa2_state, Tezos.get_self_address (), kit) in

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

let entrypoint_liquidation_auction_claim_win (state, auction_id: checker * liquidation_auction_id) : (operation list * checker) =

  let _ = ensure_valid_avl_ptr state.liquidation_auctions.avl_storage auction_id in
  let (tok, liquidation_auctions) = liquidation_auction_claim_win state.liquidation_auctions auction_id in
  let transfer =
    { from_ = Tezos.get_self_address (); (* from: FA2 account of the checker contract *)
      txs = [
        { to_ = Tezos.get_sender ();     (* to: FA2 account of the auction winner *)
          token_id = tok_token_id;
          amount = tok_to_denomination_nat tok;
        }
      ];
    } in
  let op =
    Tezos.transaction
      [transfer] (0mutez)
      (get_transfer_collateral_fa2_entrypoint state.external_contracts) in

  let state = {state with liquidation_auctions = liquidation_auctions } in


  ([op], state)

(* ************************************************************************* *)
(**                              TOUCHING                                    *)
(* ************************************************************************* *)

(** Calculate how much is right now the reward for touching the main checker
  * contract. We use a bracketed calculation, where for the first
  * touch_reward_low_bracket seconds the reward increases by touch_low_reward
  * per second, and after that by touch_high_reward per second. *)
let calculate_touch_reward (last_touched: timestamp) : kit =
  let duration_in_seconds = sub_timestamp_timestamp (Tezos.get_now ()) last_touched in
  let low_duration = min_int duration_in_seconds touch_reward_low_bracket in
  let high_duration =
    max_int
      ((0))
      (sub_int_int duration_in_seconds touch_reward_low_bracket) in

  (* reward = FLOOR (low_duration * touch_low_reward + high_duration * touch_high_reward) *)
  let { num = num_tlr; den = den_tlr; } = touch_low_reward in
  let { num = num_thr; den = den_thr; } = touch_high_reward in
  kit_of_fraction_floor
    (add_int_int
       (mul_int_int low_duration  (mul_int_int num_tlr den_thr))
       (mul_int_int high_duration (mul_int_int num_thr den_tlr))
    )
    (mul_int_int den_tlr den_thr)

(* Note that the list of operations returned is in reverse order (with respect
 * to the order the input slices were processed in). However, since the
 * operations computed are independent from each other, this needs not be a
 * problem. *)
let rec touch_oldest
    (ops, state_liquidation_auctions, state_burrows, state_parameters, state_fa2_state, maximum
                                                                                        : operation list * liquidation_auctions * burrow_map * parameters * fa2_state * nat)
  : (operation list * liquidation_auctions * burrow_map * parameters * fa2_state) =
  match is_nat (sub_nat_nat maximum (1n)) with
  | None -> (ops, state_liquidation_auctions, state_burrows, state_parameters, state_fa2_state)
  | Some maximum ->
    begin
      match liquidation_auction_oldest_completed_liquidation_slice state_liquidation_auctions with
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
[@inline] let touch_with_index (state: checker) (index: fixedpoint) : (operation list * checker) =

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

  if state_parameters.last_touched = Tezos.get_now () then
    (* Do nothing if up-to-date (idempotence) *)
    (([]: operation list), state)
  else
    (* TODO: What is the right order in which to do things here? *)

    (* 1: Mint some kit out of thin air to reward the contract toucher, and
     * update the circulating kit accordingly.*)
    let reward = calculate_touch_reward state_parameters.last_touched in
    let state_parameters = add_circulating_kit state_parameters reward in
    let state_fa2_state = ledger_issue_kit (state_fa2_state, Tezos.get_sender (), reward) in

    (* 2: Update the system parameters and add accrued burrowing fees to the
     * cfmm sub-contract. *)
    let kit_in_tok_in_prev_block, ops = calculate_kit_in_tok state_cfmm state_last_ctez_in_tez state_external_contracts in
    let total_accrual_to_cfmm, state_parameters = parameters_touch index kit_in_tok_in_prev_block state_parameters in
    (* Note: state_parameters.circulating kit here already includes the accrual to the CFMM. *)
    let state_cfmm = cfmm_add_accrued_kit state_cfmm total_accrual_to_cfmm in
    let state_fa2_state = ledger_issue_kit (state_fa2_state, Tezos.get_self_address (), total_accrual_to_cfmm) in

    (* 3: Update auction-related info (e.g. start a new auction). Note that we
     * always start auctions using the current liquidation price. We could also
     * have calculated the price right now directly using the oracle feed as
     * (tz_t * q_t), or use the current minting price, but using the
     * liquidation price is the safest option. *)
    let state_liquidation_auctions =
      liquidation_auction_touch state_liquidation_auctions (liquidation_price state_parameters) in

    (* 4: Touch oldest liquidation slices *)
    (* TODO: Touch only runs at most once per block. But it might be beneficial
     * to run this step without that restriction. *)

    (* Create an operation to ask the oracles to send updated values. This
       should be the last operation we emit, so that the system parameters do
       not change between touching different slices. *)
    let op_oracle =
      let cb = match (Tezos.get_entrypoint_opt "%receive_price" (Tezos.get_self_address ()) : ((nat * nat) contract) option) with
        | Some cb -> cb
        | None -> (failwith error_GetEntrypointOptFailureReceivePrice : (nat * nat) contract) in
      Tezos.transaction
        cb
        (0mutez)
        (get_oracle_entrypoint state_external_contracts) in
    let ops = (op_oracle :: ops) in

    (* TODO: Figure out how many slices we can process per checker entrypoint_touch.*)
    let ops, state_liquidation_auctions, state_burrows, state_parameters, state_fa2_state =
      touch_oldest (ops, state_liquidation_auctions, state_burrows, state_parameters, state_fa2_state, number_of_slices_to_process) in

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

let entrypoint_touch (state, _: checker * unit) : (operation list * checker) =
  let index = match state.last_index with
    | None -> state.parameters.index (* use the old one *)
    | Some i -> i in (* tez/chf (or chf in tez) *)
  touch_with_index state index

(* ************************************************************************* *)
(**                               ORACLE                                     *)
(* ************************************************************************* *)

let entrypoint_receive_price (state, oracle_price: checker * (nat * nat)) : (operation list * checker) =

  if Tezos.get_sender () <> state.external_contracts.oracle then
    (failwith error_UnauthorisedCaller : operation list * checker)
  else
    (* NOTE: By storing the price as a fixedpoint we lose some precision here.
     * However, keeping the original fraction here could become much more
     * costly in other places (in the calculation of the index and the
     * protected index, for example). Another alternative (with more-or-less
     * the same effect) would be to keep the last_index field as a fraction and
     * convert it in the call to touch. *)
    let price =
      let (num, den) = oracle_price in
      fixedpoint_of_ratio_floor (make_ratio (int num) (int den)) in
    (([]: operation list), {state with last_index = Some price})

let entrypoint_receive_ctez_marginal_price (state, price: checker * (nat * nat)) : (operation list * checker) =

  if Tezos.get_sender () <> state.external_contracts.ctez_cfmm then
    (failwith error_UnauthorisedCaller : operation list * checker)
  else
    let num_tez, den_ctez = price in
    let state_last_ctez_in_tez = { num = int num_tez; den = int den_ctez; } in
    (([]: operation list), {state with last_ctez_in_tez = Some state_last_ctez_in_tez})

(* ************************************************************************* *)
(**                               FA2                                        *)
(* ************************************************************************* *)

let strict_entrypoint_transfer (state, xs: checker * fa2_transfer list) : (operation list * checker) =

  let state = { state with fa2_state = fa2_run_transfer (state.fa2_state, xs) } in

  (([]: operation list), state)

[@inline] let strict_entrypoint_balance_of (state, param: checker * fa2_balance_of_param) : (operation list * checker) =

  let { requests = requests; callback = callback; } = param in
  let response = fa2_run_balance_of (state.fa2_state, requests) in
  let op = Tezos.transaction response (0mutez) callback in

  ([op], state)

let entrypoint_update_operators (state, xs: checker * fa2_update_operator list) : (operation list * checker) =

  let state = { state with fa2_state = fa2_run_update_operators (state.fa2_state, xs) } in

  (([]: operation list), state)

(* ************************************************************************* *)
(**                               VIEWS                                      *)
(* ************************************************************************* *)

let view_buy_kit_min_kit_expected (ctok, state: ctok * checker) : kit =

  let (kit, _cfmm) = cfmm_view_min_kit_expected_buy_kit state.cfmm state.parameters.target ctok in
  kit

let view_sell_kit_min_ctok_expected (kit, state: kit * checker) : ctok =

  let (ctok, _cfmm) = cfmm_view_min_ctok_expected_cfmm_sell_kit state.cfmm state.parameters.target kit in
  ctok

let view_add_liquidity_max_kit_deposited (ctok, state: ctok * checker) : kit =

  let (_lqt, kit, _cfmm) = cfmm_view_max_kit_deposited_min_lqt_minted_cfmm_add_liquidity state.cfmm ctok in
  kit

let view_add_liquidity_min_lqt_minted (ctok, state: ctok * checker) : lqt =

  let (lqt, _kit, _cfmm) = cfmm_view_max_kit_deposited_min_lqt_minted_cfmm_add_liquidity state.cfmm ctok in
  lqt

let view_remove_liquidity_min_ctok_withdrawn (lqt, state: lqt * checker) : ctok =

  let (ctok, _kit, _cfmm) = cfmm_view_min_ctok_withdrawn_min_kit_withdrawn_cfmm_remove_liquidity state.cfmm lqt in
  ctok

let view_remove_liquidity_min_kit_withdrawn (lqt, state: lqt * checker) : kit =

  let (_ctok, kit, _cfmm) = cfmm_view_min_ctok_withdrawn_min_kit_withdrawn_cfmm_remove_liquidity state.cfmm lqt in
  kit

let view_burrow_max_mintable_kit (burrow_id, state: burrow_id * checker) : kit =

  let burrow = find_burrow state.burrows burrow_id in
  let burrow = burrow_touch state.parameters burrow in
  burrow_max_mintable_kit state.parameters burrow

let view_is_burrow_overburrowed (burrow_id, state: burrow_id * checker) : bool =

  let burrow = find_burrow state.burrows burrow_id in
  let burrow = burrow_touch state.parameters burrow in
  burrow_is_overburrowed state.parameters burrow

let view_is_burrow_liquidatable (burrow_id, state: burrow_id * checker) : bool =

  let burrow = find_burrow state.burrows burrow_id in
  let burrow = burrow_touch state.parameters burrow in
  burrow_is_liquidatable state.parameters burrow

let view_current_liquidation_auction_details ((), state: unit * checker) : view_current_liquidation_auction_details_result =

  let auction = liquidation_auction_get_current_auction state.liquidation_auctions in
  let current_bid, blocks, seconds = match auction.state with
    | Descending _ -> (None: (bid option)), (None: (int option)), (None: (int option))
    | Ascending (current_bid, bid_time_seconds, bid_level) ->
      let level = Tezos.get_level () in
      let now = Tezos.get_now () in
      (
        Some current_bid,
        Some (sub_nat_nat (add_nat_nat bid_level max_bid_interval_in_blocks) level),
        Some (sub_timestamp_timestamp (add_timestamp_int bid_time_seconds max_bid_interval_in_seconds) now)
      )
  in
  {
    auction_id = auction.contents;
    collateral = avl_tok state.liquidation_auctions.avl_storage auction.contents;
    minimum_bid = liquidation_auction_current_auction_minimum_bid auction;
    current_bid = current_bid;
    remaining_blocks = blocks;
    remaining_seconds = seconds;
  }

(* ************************************************************************* *)
(**                            FA2_VIEWS                                     *)
(* ************************************************************************* *)

let view_get_balance ((owner, token_id), state: (address * fa2_token_id) * checker) : nat =

  fa2_get_balance (state.fa2_state, owner, token_id)

let view_total_supply (token_id, state: fa2_token_id * checker) : nat =

  if token_id = kit_token_id then
    kit_to_denomination_nat state.parameters.circulating_kit
  else if token_id = lqt_token_id then
    lqt_to_denomination_nat (lqt_sub state.cfmm.lqt (lqt_of_denomination (1n)))
  else
    failwith "FA2_TOKEN_UNDEFINED"

let view_all_tokens ((), _state: unit * checker) : fa2_token_id list =

  fa2_all_tokens

let view_is_operator ((owner, (operator, token_id)), state: (address * (address * fa2_token_id)) * checker): bool =

  fa2_is_operator (state.fa2_state, operator, owner, token_id)
