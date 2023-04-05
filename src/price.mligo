#import "./cfmm.mligo" "CFMM"
#import "./cfmmTypes.mligo" "CFMMT"
#import "./common.mligo" "Common"
#import "./checkerTypes.mligo" "CheckerT"
#include "./error.mligo"

(* If COLLATERAL = TEZ then
 *   KIT/CHF = TEZ/CHF (from index)
 *           * CTEZ/TEZ (from ctez)
 *           * KIT/CTEZ (from cfmm)
*)
[@inline] let calculate_kit_in_tok
    (state_cfmm: CFMMT.cfmm)
    (state_last_ctez_in_tez: Common.ratio option)
    (state_external_contracts: CheckerT.external_contracts)
  : (Common.ratio * operation list) =
  (* 1. Get the price of kit in ctez from the cfmm. To avoid having cfmm users
   *    trying to manipulate the price, we use the last price of kit in ctez
   *    observed, not the one in the current block. *)
  let { num = num_ctez; den = den_kit; } = CFMM.cfmm_kit_in_ctok_in_prev_block state_cfmm in
  (* 2. Get the price of ctez in tez from storage (last observed). Use tez/ctez
   *    = 1 as the default price if none was observed. *)
  let { num = num_tez; den = den_ctez; } = match state_last_ctez_in_tez with
    | None -> Common.one_ratio
    | Some price -> price in
  (* 3. kit_in_tez = kit_in_ctez * ctez_in_tez *)
  let price =
    { num = num_ctez * num_tez;
      den = den_kit * den_ctez;
    } in

  (* Create an operation to ask the ctez cfmm to send updated values. Emit
   * this operation next to the one requesting prices from oracles, at the
   * end, so that the system parameters do not change between touching
   * different slices. *)
  let op_ctez_price =
    let self_address = Tezos.get_self_address () in
    let cb = match (Tezos.get_entrypoint_opt "%receive_ctez_marginal_price" self_address : ((nat * nat) contract) option) with
      | Some cb -> cb
      | None -> (failwith error_GetEntrypointOptFailureReceiveCtezMarginalPrice : (nat * nat) contract) in
    Tezos.transaction
      cb
      (0mutez)
      (CheckerT.get_ctez_cfmm_price_entrypoint state_external_contracts) in

  (price, [op_ctez_price])
