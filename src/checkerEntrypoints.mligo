#import "./checkerTypes.mligo" "CheckerT"
#import "./checker.mligo" "Checker"
#import "./fa2Interface.mligo" "FA2"
#import "./cfmm.mligo" "CFMM"
#import "./fixedPoint.mligo" "FixedPoint"
#import "./kit.mligo" "Kit"
#import "./lqt.mligo" "Lqt"
#import "./tok.mligo" "Tok"
#import "./ctok.mligo" "Ctok"
#import "./liquidationAuctionTypes.mligo" "Liquidation"
#import "./fa2Interface.mligo" "FA2"
#include "./error.mligo"
(* open Error *)

type lazy_params =
  | Touch of unit
  | Create_burrow of (nat * key_hash option * Tok.tok)
  | Deposit_collateral of (nat * Tok.tok)
  | Withdraw_collateral of (nat * Tok.tok)
  | Mint_kit of (nat * Kit.kit)
  | Burn_kit of (nat * Kit.kit)
  | Activate_burrow of (nat * Tok.tok)
  | Deactivate_burrow of (nat * address)
  | Mark_for_liquidation of CheckerT.burrow_id
  | Touch_liquidation_slices of Liquidation.leaf_ptr list
  | Cancel_liquidation_slice of Liquidation.leaf_ptr
  | Touch_burrow of CheckerT.burrow_id
  | Set_burrow_delegate of (nat * key_hash option)
  | Buy_kit of (Ctok.ctok * Kit.kit * timestamp)
  | Sell_kit of (Kit.kit * Ctok.ctok * timestamp)
  | Add_liquidity of (Ctok.ctok * Kit.kit * Lqt.lqt * timestamp)
  | Remove_liquidity of (Lqt.lqt * Ctok.ctok * Kit.kit * timestamp)
  | Liquidation_auction_place_bid of (Liquidation.liquidation_auction_id * Kit.kit)
  | Liquidation_auction_claim_win of Liquidation.liquidation_auction_id
  | Receive_price of (nat * nat)
  | Receive_ctez_marginal_price of (nat * nat)
  | Update_operators of FA2.fa2_update_operator list

[@inline] let lazy_id_touch = (0)
[@inline] let lazy_id_create_burrow = (1)
[@inline] let lazy_id_deposit_collateral = (2)
[@inline] let lazy_id_withdraw_collateral = (3)
[@inline] let lazy_id_mint_kit = (4)
[@inline] let lazy_id_burn_kit = (5)
[@inline] let lazy_id_activate_burrow = (6)
[@inline] let lazy_id_deactivate_burrow = (7)
[@inline] let lazy_id_mark_for_liquidation = (8)
[@inline] let lazy_id_touch_liquidation_slices = (9)
[@inline] let lazy_id_cancel_liquidation_slice = (10)
[@inline] let lazy_id_touch_burrow = (11)
[@inline] let lazy_id_set_burrow_delegate = (12)
[@inline] let lazy_id_buy_kit = (13)
[@inline] let lazy_id_sell_kit = (14)
[@inline] let lazy_id_add_liquidity = (15)
[@inline] let lazy_id_remove_liquidity = (16)
[@inline] let lazy_id_liquidation_auction_place_bid = (17)
[@inline] let lazy_id_liquidation_auction_claim_win = (18)
[@inline] let lazy_id_receive_price = (19)
[@inline] let lazy_id_receive_ctez_marginal_price = (20)
[@inline] let lazy_id_update_operators = (21)

type lazy_function = CheckerT.checker * bytes -> operation list * CheckerT.checker

let lazyParamsToLazyFunctionId (p: lazy_params) : CheckerT.lazy_function_id * bytes =
  match p with
  | Touch a -> (lazy_id_touch, Bytes.pack a)
  | Create_burrow a -> (lazy_id_create_burrow, Bytes.pack a)
  | Deposit_collateral a -> (lazy_id_deposit_collateral, Bytes.pack a)
  | Withdraw_collateral a -> (lazy_id_withdraw_collateral, Bytes.pack a)
  | Mint_kit a -> (lazy_id_mint_kit, Bytes.pack a)
  | Burn_kit a -> (lazy_id_burn_kit, Bytes.pack a)
  | Activate_burrow a -> (lazy_id_activate_burrow, Bytes.pack a)
  | Deactivate_burrow a -> (lazy_id_deactivate_burrow, Bytes.pack a)
  | Mark_for_liquidation a -> (lazy_id_mark_for_liquidation, Bytes.pack a)
  | Touch_liquidation_slices a -> (lazy_id_touch_liquidation_slices, Bytes.pack a)
  | Cancel_liquidation_slice a -> (lazy_id_cancel_liquidation_slice, Bytes.pack a)
  | Touch_burrow a -> (lazy_id_touch_burrow, Bytes.pack a)
  | Set_burrow_delegate a -> (lazy_id_set_burrow_delegate, Bytes.pack a)
  | Buy_kit a -> (lazy_id_buy_kit, Bytes.pack a)
  | Sell_kit a -> (lazy_id_sell_kit, Bytes.pack a)
  | Add_liquidity a -> (lazy_id_add_liquidity, Bytes.pack a)
  | Remove_liquidity a -> (lazy_id_remove_liquidity, Bytes.pack a)
  | Liquidation_auction_place_bid a -> (lazy_id_liquidation_auction_place_bid, Bytes.pack a)
  | Liquidation_auction_claim_win a -> (lazy_id_liquidation_auction_claim_win, Bytes.pack a)
  | Receive_price a -> (lazy_id_receive_price, Bytes.pack a)
  | Receive_ctez_marginal_price a -> (lazy_id_receive_ctez_marginal_price, Bytes.pack a)
  | Update_operators a -> (lazy_id_update_operators, Bytes.pack a)

(* BEGIN_LIGO *)
   let lazy_fun_touch (checker, params: CheckerT.checker * bytes): operation list * CheckerT.checker =
   match (Bytes.unpack params: unit option) with
   | Some p -> Checker.entrypoint_touch (checker, p)
   | None -> (failwith error_UnexpectedParams : operation list * CheckerT.checker)
   (* END_LIGO *)

(* BEGIN_LIGO *)
   let lazy_fun_create_burrow (checker, params: CheckerT.checker * bytes): operation list * CheckerT.checker =
   match (Bytes.unpack params: (nat * key_hash option * Tok.tok) option) with
   | Some p -> Checker.entrypoint_create_burrow (checker, p)
   | None -> (failwith error_UnexpectedParams : operation list * CheckerT.checker)
   (* END_LIGO *)

(* BEGIN_LIGO *)
   let lazy_fun_deposit_collateral (checker, params: CheckerT.checker * bytes): operation list * CheckerT.checker =
   match (Bytes.unpack params: (nat * Tok.tok) option) with
   | Some p -> Checker.entrypoint_deposit_collateral (checker, p)
   | None -> (failwith error_UnexpectedParams : operation list * CheckerT.checker)
   (* END_LIGO *)

(* BEGIN_LIGO *)
   let lazy_fun_withdraw_collateral (checker, params: CheckerT.checker * bytes): operation list * CheckerT.checker =
   match (Bytes.unpack params: (nat * Tok.tok) option) with
   | Some p -> Checker.entrypoint_withdraw_collateral (checker, p)
   | None -> (failwith error_UnexpectedParams : operation list * CheckerT.checker)
   (* END_LIGO *)

(* BEGIN_LIGO *)
   let lazy_fun_mint_kit (checker, params: CheckerT.checker * bytes): operation list * CheckerT.checker =
   match (Bytes.unpack params: (nat * Kit.kit) option) with
   | Some p -> Checker.entrypoint_mint_kit (checker, p)
   | None -> (failwith error_UnexpectedParams : operation list * CheckerT.checker)
   (* END_LIGO *)

(* BEGIN_LIGO *)
   let lazy_fun_burn_kit (checker, params: CheckerT.checker * bytes): operation list * CheckerT.checker =
   match (Bytes.unpack params: (nat * Kit.kit) option) with
   | Some p -> Checker.entrypoint_burn_kit (checker, p)
   | None -> (failwith error_UnexpectedParams : operation list * CheckerT.checker)
   (* END_LIGO *)

(* BEGIN_LIGO *)
   let lazy_fun_activate_burrow (checker, params: CheckerT.checker * bytes): operation list * CheckerT.checker =
   match (Bytes.unpack params: (nat * Tok.tok) option) with
   | Some p -> Checker.entrypoint_activate_burrow (checker, p)
   | None -> (failwith error_UnexpectedParams : operation list * CheckerT.checker)
   (* END_LIGO *)

(* BEGIN_LIGO *)
   let lazy_fun_deactivate_burrow (checker, params: CheckerT.checker * bytes): operation list * CheckerT.checker =
   match (Bytes.unpack params: (nat * address) option) with
   | Some p -> Checker.entrypoint_deactivate_burrow (checker, p)
   | None -> (failwith error_UnexpectedParams : operation list * CheckerT.checker)
   (* END_LIGO *)

(* BEGIN_LIGO *)
   let lazy_fun_mark_for_liquidation (checker, params: CheckerT.checker * bytes): operation list * CheckerT.checker =
   match (Bytes.unpack params: CheckerT.burrow_id option) with
   | Some p -> Checker.entrypoint_mark_for_liquidation (checker, p)
   | None -> (failwith error_UnexpectedParams : operation list * CheckerT.checker)
   (* END_LIGO *)

(* BEGIN_LIGO *)
   let lazy_fun_touch_liquidation_slices (checker, params: CheckerT.checker * bytes): operation list * CheckerT.checker =
   match (Bytes.unpack params: Liquidation.leaf_ptr list option) with
   | Some p -> Checker.entrypoint_touch_liquidation_slices (checker, p)
   | None -> (failwith error_UnexpectedParams : operation list * CheckerT.checker)
   (* END_LIGO *)

(* BEGIN_LIGO *)
   let lazy_fun_cancel_liquidation_slice (checker, params: CheckerT.checker * bytes): operation list * CheckerT.checker =
   match (Bytes.unpack params: Liquidation.leaf_ptr option) with
   | Some p -> Checker.entrypoint_cancel_liquidation_slice (checker, p)
   | None -> (failwith error_UnexpectedParams : operation list * CheckerT.checker)
   (* END_LIGO *)

(* BEGIN_LIGO *)
   let lazy_fun_touch_burrow (checker, params: CheckerT.checker * bytes): operation list * CheckerT.checker =
   match (Bytes.unpack params: CheckerT.burrow_id option) with
   | Some p -> Checker.entrypoint_touch_burrow (checker, p)
   | None -> (failwith error_UnexpectedParams : operation list * CheckerT.checker)
   (* END_LIGO *)

(* BEGIN_LIGO *)
   let lazy_fun_set_burrow_delegate (checker, params: CheckerT.checker * bytes): operation list * CheckerT.checker =
   match (Bytes.unpack params: (nat * key_hash option) option) with
   | Some p -> Checker.entrypoint_set_burrow_delegate (checker, p)
   | None -> (failwith error_UnexpectedParams : operation list * CheckerT.checker)
   (* END_LIGO *)

(* BEGIN_LIGO *)
   let lazy_fun_buy_kit (checker, params: CheckerT.checker * bytes): operation list * CheckerT.checker =
   match (Bytes.unpack params: (Ctok.ctok * Kit.kit * timestamp) option) with
   | Some p -> Checker.entrypoint_buy_kit (checker, p)
   | None -> (failwith error_UnexpectedParams : operation list * CheckerT.checker)
   (* END_LIGO *)

(* BEGIN_LIGO *)
   let lazy_fun_sell_kit (checker, params: CheckerT.checker * bytes): operation list * CheckerT.checker =
   match (Bytes.unpack params: (Kit.kit * Ctok.ctok * timestamp) option) with
   | Some p -> Checker.entrypoint_sell_kit (checker, p)
   | None -> (failwith error_UnexpectedParams : operation list * CheckerT.checker)
   (* END_LIGO *)

(* BEGIN_LIGO *)
   let lazy_fun_add_liquidity (checker, params: CheckerT.checker * bytes): operation list * CheckerT.checker =
   match (Bytes.unpack params: (Ctok.ctok * Kit.kit * Lqt.lqt * timestamp) option) with
   | Some p -> Checker.entrypoint_add_liquidity (checker, p)
   | None -> (failwith error_UnexpectedParams : operation list * CheckerT.checker)
   (* END_LIGO *)

(* BEGIN_LIGO *)
   let lazy_fun_remove_liquidity (checker, params: CheckerT.checker * bytes): operation list * CheckerT.checker =
   match (Bytes.unpack params: (Lqt.lqt * Ctok.ctok * Kit.kit * timestamp) option) with
   | Some p -> Checker.entrypoint_remove_liquidity (checker, p)
   | None -> (failwith error_UnexpectedParams : operation list * CheckerT.checker)
   (* END_LIGO *)

(* BEGIN_LIGO *)
   let lazy_fun_liquidation_auction_place_bid (checker, params: CheckerT.checker * bytes): operation list * CheckerT.checker =
   match (Bytes.unpack params: (Liquidation.liquidation_auction_id * Kit.kit) option) with
   | Some p -> Checker.entrypoint_liquidation_auction_place_bid (checker, p)
   | None -> (failwith error_UnexpectedParams : operation list * CheckerT.checker)
   (* END_LIGO *)

(* BEGIN_LIGO *)
   let lazy_fun_liquidation_auction_claim_win (checker, params: CheckerT.checker * bytes): operation list * CheckerT.checker =
   match (Bytes.unpack params: Liquidation.liquidation_auction_id option) with
   | Some p -> Checker.entrypoint_liquidation_auction_claim_win (checker, p)
   | None -> (failwith error_UnexpectedParams : operation list * CheckerT.checker)
   (* END_LIGO *)

(* BEGIN_LIGO *)
   let lazy_fun_receive_price (checker, params: CheckerT.checker * bytes): operation list * CheckerT.checker =
   match (Bytes.unpack params: (nat * nat) option) with
   | Some p -> Checker.entrypoint_receive_price (checker, p)
   | None -> (failwith error_UnexpectedParams : operation list * CheckerT.checker)
   (* END_LIGO *)

(* BEGIN_LIGO *)
   let lazy_fun_receive_ctez_marginal_price (checker, params: CheckerT.checker * bytes): operation list * CheckerT.checker =
   match (Bytes.unpack params: (nat * nat) option) with
   | Some p -> Checker.entrypoint_receive_ctez_marginal_price (checker, p)
   | None -> (failwith error_UnexpectedParams : operation list * CheckerT.checker)
   (* END_LIGO *)

(* BEGIN_LIGO *)
   let lazy_fun_update_operators (checker, params: CheckerT.checker * bytes): operation list * CheckerT.checker =
   match (Bytes.unpack params: FA2.fa2_update_operator list option) with
   | Some p -> Checker.entrypoint_update_operators (checker, p)
   | None -> (failwith error_UnexpectedParams : operation list * CheckerT.checker)
   (* END_LIGO *)


let wrapper_view_buy_kit_min_kit_expected (param, wrapper: Ctok.ctok * CheckerT.wrapper): Kit.kit =
  match wrapper.deployment_state with
  | Sealed checker -> Checker.view_buy_kit_min_kit_expected (param, checker)
  | Unsealed _ -> (failwith error_ContractNotDeployed: Kit.kit)

let wrapper_view_sell_kit_min_ctok_expected (param, wrapper: Kit.kit * CheckerT.wrapper): Ctok.ctok =
  match wrapper.deployment_state with
  | Sealed checker -> Checker.view_sell_kit_min_ctok_expected (param, checker)
  | Unsealed _ -> (failwith error_ContractNotDeployed: Ctok.ctok)

let wrapper_view_add_liquidity_max_kit_deposited (param, wrapper: Ctok.ctok * CheckerT.wrapper): Kit.kit =
  match wrapper.deployment_state with
  | Sealed checker -> Checker.view_add_liquidity_max_kit_deposited (param, checker)
  | Unsealed _ -> (failwith error_ContractNotDeployed: Kit.kit)

let wrapper_view_add_liquidity_min_lqt_minted (param, wrapper: Ctok.ctok * CheckerT.wrapper): Lqt.lqt =
  match wrapper.deployment_state with
  | Sealed checker -> Checker.view_add_liquidity_min_lqt_minted (param, checker)
  | Unsealed _ -> (failwith error_ContractNotDeployed: Lqt.lqt)

let wrapper_view_remove_liquidity_min_ctok_withdrawn (param, wrapper: Lqt.lqt * CheckerT.wrapper): Ctok.ctok =
  match wrapper.deployment_state with
  | Sealed checker -> Checker.view_remove_liquidity_min_ctok_withdrawn (param, checker)
  | Unsealed _ -> (failwith error_ContractNotDeployed: Ctok.ctok)

let wrapper_view_remove_liquidity_min_kit_withdrawn (param, wrapper: Lqt.lqt * CheckerT.wrapper): Kit.kit =
  match wrapper.deployment_state with
  | Sealed checker -> Checker.view_remove_liquidity_min_kit_withdrawn (param, checker)
  | Unsealed _ -> (failwith error_ContractNotDeployed: Kit.kit)

let wrapper_view_current_liquidation_auction_details (param, wrapper: unit * CheckerT.wrapper): CheckerT.view_current_liquidation_auction_details_result =
  match wrapper.deployment_state with
  | Sealed checker -> Checker.view_current_liquidation_auction_details (param, checker)
  | Unsealed _ -> (failwith error_ContractNotDeployed: CheckerT.view_current_liquidation_auction_details_result)

let wrapper_view_burrow_max_mintable_kit (param, wrapper: CheckerT.burrow_id * CheckerT.wrapper): Kit.kit =
  match wrapper.deployment_state with
  | Sealed checker -> Checker.view_burrow_max_mintable_kit (param, checker)
  | Unsealed _ -> (failwith error_ContractNotDeployed: Kit.kit)

let wrapper_view_is_burrow_overburrowed (param, wrapper: CheckerT.burrow_id * CheckerT.wrapper): bool =
  match wrapper.deployment_state with
  | Sealed checker -> Checker.view_is_burrow_overburrowed (param, checker)
  | Unsealed _ -> (failwith error_ContractNotDeployed: bool)

let wrapper_view_is_burrow_liquidatable (param, wrapper: CheckerT.burrow_id * CheckerT.wrapper): bool =
  match wrapper.deployment_state with
  | Sealed checker -> Checker.view_is_burrow_liquidatable (param, checker)
  | Unsealed _ -> (failwith error_ContractNotDeployed: bool)

let wrapper_view_get_balance (param, wrapper: (address * FA2.fa2_token_id) * CheckerT.wrapper): nat =
  match wrapper.deployment_state with
  | Sealed checker -> Checker.view_get_balance (param, checker)
  | Unsealed _ -> (failwith error_ContractNotDeployed: nat)

let wrapper_view_total_supply (param, wrapper: FA2.fa2_token_id * CheckerT.wrapper): nat =
  match wrapper.deployment_state with
  | Sealed checker -> Checker.view_total_supply (param, checker)
  | Unsealed _ -> (failwith error_ContractNotDeployed: nat)

let wrapper_view_all_tokens (param, wrapper: unit * CheckerT.wrapper): FA2.fa2_token_id list =
  match wrapper.deployment_state with
  | Sealed checker -> Checker.view_all_tokens (param, checker)
  | Unsealed _ -> (failwith error_ContractNotDeployed: FA2.fa2_token_id list)

let wrapper_view_is_operator (param, wrapper: (address * (address * FA2.fa2_token_id)) * CheckerT.wrapper): bool =
  match wrapper.deployment_state with
  | Sealed checker -> Checker.view_is_operator (param, checker)
  | Unsealed _ -> (failwith error_ContractNotDeployed: bool)
