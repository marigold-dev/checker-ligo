#import "./burrow.mligo" "Burrow"
#import "./cfmm.mligo" "CFMM"
#import "./tok.mligo" "Tok"
#import "./kit.mligo" "Kit"
#import "./cfmmTypes.mligo" "CFMMTypes"
#import "./fa2Ledger.mligo" "FA2"
#import "./fixedPoint.mligo" "FixedPoint"
#import "./common.mligo" "Common"
#import "./parameters.mligo" "Parameters"
#import "./liquidationAuctionTypes.mligo" "Liquidation"
#include "./error.mligo"

type burrow_id = address * nat

type burrow_map = (burrow_id, Burrow.burrow) big_map

type external_contracts = {
  oracle : address;
  collateral_fa2 : address;
  ctok_fa2 : address;
  ctez_cfmm : address;
}

type checker =
  { burrows : burrow_map;
    cfmm : CFMMTypes.cfmm;
    parameters : Parameters.t;
    liquidation_auctions : Liquidation.liquidation_auctions;
    last_index : FixedPoint.fixedpoint option;  (* FIXME: unused at the moment. *)
    last_ctez_in_tez : Common.ratio option;
    fa2_state : FA2.fa2_state;
    external_contracts : external_contracts;
  }

(** Make a fresh state. *)
let initial_checker (external_contracts: external_contracts) =
  { burrows = (Big_map.empty: (burrow_id, Burrow.burrow) big_map);
    cfmm = CFMM.initial_cfmm ();
    parameters = Parameters.initial_parameters;
    liquidation_auctions = Liquidation.liquidation_auction_empty;
    last_index = (None : FixedPoint.fixedpoint option);
    last_ctez_in_tez = (None : Common.ratio option);
    fa2_state = FA2.initial_fa2_state;
    external_contracts = external_contracts;
  }

type lazy_function_id = int

type deployment_state =
  | Unsealed of address
  | Sealed of checker

type lazy_function_map = (lazy_function_id, bytes) big_map
type wrapper =
  [@layout:comb]
  { lazy_functions : lazy_function_map
  ; metadata: (string, bytes) big_map
  ; deployment_state : deployment_state
  }

type view_current_liquidation_auction_details_result =
  { auction_id: Liquidation.liquidation_auction_id
  ; collateral: Tok.tok
  ; minimum_bid: Kit.kit
  ; current_bid: Liquidation.bid option
  ; remaining_blocks: int option
  ; remaining_seconds: int option
  }

(* ************************************************************************* *)
(**                           EXTERNAL_CONTRACTS                             *)
(* ************************************************************************* *)

[@inline] let get_transfer_ctok_fa2_entrypoint (external_contracts: external_contracts): (FA2.fa2_transfer list) contract =
  match (Tezos.get_entrypoint_opt "%transfer" external_contracts.ctok_fa2 : (FA2.fa2_transfer list) contract option) with
  | Some c -> c
  | None -> (failwith error_GetEntrypointOptFailureFA2Transfer : (FA2.fa2_transfer list) contract)

[@inline] let get_ctez_cfmm_price_entrypoint (external_contracts: external_contracts): ((nat * nat) contract) contract =
  match (Tezos.get_entrypoint_opt "%getMarginalPrice" external_contracts.ctez_cfmm : ((nat * nat) contract) contract option) with
  | Some c -> c
  | None -> (failwith error_GetEntrypointOptFailureCtezGetMarginalPrice : ((nat * nat) contract) contract)

[@inline] let get_transfer_collateral_fa2_entrypoint (external_contracts: external_contracts): (FA2.fa2_transfer list) contract =
  match (Tezos.get_entrypoint_opt "%transfer" external_contracts.collateral_fa2 : (FA2.fa2_transfer list) contract option) with
  | Some c -> c
  | None -> (failwith error_GetEntrypointOptFailureFA2Transfer : (FA2.fa2_transfer list) contract)
