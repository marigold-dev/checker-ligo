#include "./checkerMain.mligo"

(* FIXME code duplication because of a limitation of the preprocessor: we just call the
 * lazy params directly instead of looking up in the big_map *)

let run_lazy_params
  (params: Entrypoints.lazy_params)
  (checker: CheckerT.checker): operation list * CheckerT.checker =
  match params with
  | Touch a -> Checker.entrypoint_touch (checker, a)
  | Create_burrow a -> Checker.entrypoint_create_burrow (checker, a)
  | Deposit_collateral a -> Checker.entrypoint_deposit_collateral (checker, a)
  | Withdraw_collateral a -> Checker.entrypoint_withdraw_collateral (checker, a)
  | Mint_kit a -> Checker.entrypoint_mint_kit (checker, a)
  | Burn_kit a -> Checker.entrypoint_burn_kit (checker, a)
  | Activate_burrow a -> Checker.entrypoint_activate_burrow (checker, a)
  | Deactivate_burrow a -> Checker.entrypoint_deactivate_burrow (checker, a)
  | Mark_for_liquidation a -> Checker.entrypoint_mark_for_liquidation (checker, a)
  | Touch_liquidation_slices a -> Checker.entrypoint_touch_liquidation_slices (checker, a)
  | Cancel_liquidation_slice a -> Checker.entrypoint_cancel_liquidation_slice (checker, a)
  | Touch_burrow a -> Checker.entrypoint_touch_burrow (checker, a)
  | Set_burrow_delegate a -> Checker.entrypoint_set_burrow_delegate (checker, a)
  | Buy_kit a -> Checker.entrypoint_buy_kit (checker, a)
  | Sell_kit a -> Checker.entrypoint_sell_kit (checker, a)
  | Add_liquidity a -> Checker.entrypoint_add_liquidity (checker, a)
  | Remove_liquidity a -> Checker.entrypoint_remove_liquidity (checker, a)
  | Liquidation_auction_place_bid a -> Checker.entrypoint_liquidation_auction_place_bid (checker, a)
  | Liquidation_auction_claim_win a -> Checker.entrypoint_liquidation_auction_claim_win (checker, a)
  | Receive_price a -> Checker.entrypoint_receive_price (checker, a)
  | Receive_ctez_marginal_price a -> Checker.entrypoint_receive_ctez_marginal_price (checker, a)
  | Update_operators a -> Checker.entrypoint_update_operators (checker, a)

let main (op: params) (state: CheckerT.wrapper): operation list * CheckerT.wrapper =
  let _ = Common.ensure_no_tez_given () in

  let { lazy_functions = lazy_functions; metadata = metadata; deployment_state = deployment_state } = state in

  let ops, lazy_functions, metadata, deployment_state = match deployment_state with
    | Unsealed deployer ->
      begin if Tezos.get_sender () = deployer then
          begin match op with
            | DeployFunction p ->
              let lfi, bs = p in
              let lazy_functions =
                match Big_map.find_opt lfi lazy_functions with
                | None -> Big_map.add lfi bs lazy_functions
                | Some prev -> Big_map.add lfi (Bytes.concat prev bs) lazy_functions in
              (([]: operation list), lazy_functions, metadata, Unsealed deployer)
            | DeployMetadata bs ->
              let metadata =
                match Big_map.find_opt "m" metadata with
                | None -> Big_map.add "m" bs metadata
                | Some prev -> Big_map.add "m" (Bytes.concat prev bs) metadata in
              (([]: operation list), lazy_functions, metadata, Unsealed deployer)
            | SealContract external_contracts ->
              (* check if the given oracle, collateral_fa2, and ctez contracts have the entrypoints we need *)
              let _ = Oracle.get_oracle_entrypoint external_contracts in
              let _ = CheckerT.get_transfer_collateral_fa2_entrypoint external_contracts in
              let _ = CheckerT.get_transfer_ctok_fa2_entrypoint external_contracts in
              let _ = CheckerT.get_ctez_cfmm_price_entrypoint external_contracts in

              (* emit a touch operation to checker *)
              let touchOp =
                let self_address = Tezos.get_self_address () in
                match (Tezos.get_entrypoint_opt "%touch" self_address : unit contract option) with
                | Some c -> Tezos.transaction () (0mutez) c
                | None -> (failwith ((-4)) : operation) in

              (* initialize checker state *)
              let checker = CheckerT.initial_checker external_contracts in

              (* add the metadata boilerplate *)
              (* Python: b"tezos-storage:m".hex() *)
              let metadata_url = 0x74657a6f732d73746f726167653a6d in
              let metadata = Big_map.add "" metadata_url metadata in

              ([touchOp], lazy_functions, metadata, Sealed checker)
            | CheckerEntrypoint _ ->
              (* Note: disabling coverage for the unreported but accessed right-hand side;
               * accessibility is sufficiently marked on the pattern itself. *)
              ((failwith error_ContractNotDeployed (* [@coverage off] *)): operation list * CheckerT.lazy_function_map * (string, bytes) big_map * CheckerT.deployment_state)
          end
        else
          (* Note: disabling coverage for the unreported but accessed right-hand side;
           * accessibility is sufficiently marked on the pattern itself. *)
          ((failwith error_UnauthorisedCaller (* [@coverage off] *)): operation list * CheckerT.lazy_function_map * (string, bytes) big_map * CheckerT.deployment_state)
      end
    | Sealed checker ->
      let ops, checker =
        match op with
        | DeployFunction _ ->
          (* Note: disabling coverage for the unreported but accessed right-hand side;
           * accessibility is sufficiently marked on the pattern itself. *)
          ((failwith error_ContractAlreadyDeployed (* [@coverage off] *)): operation list * CheckerT.checker)
        | SealContract _ ->
          (* Note: disabling coverage for the unreported but accessed right-hand side;
           * accessibility is sufficiently marked on the pattern itself. *)
          ((failwith error_ContractAlreadyDeployed (* [@coverage off] *)): operation list * CheckerT.checker)
        | DeployMetadata _ ->
          (* Note: disabling coverage for the unreported but accessed right-hand side;
           * accessibility is sufficiently marked on the pattern itself. *)
          ((failwith error_ContractAlreadyDeployed (* [@coverage off] *)): operation list * CheckerT.checker)
          (* [@coverage off] *)
        | CheckerEntrypoint op -> begin
            match op with
            | StrictParams op -> begin
                match op with
                | Balance_of p -> Checker.strict_entrypoint_balance_of (checker, p)
                | Transfer p -> Checker.strict_entrypoint_transfer (checker, p)
              end
            | LazyParams op ->
              run_lazy_params op checker
          end in
      (ops, lazy_functions, metadata, Sealed checker)
  in
  (ops, { lazy_functions = lazy_functions; metadata = metadata; deployment_state = deployment_state })
