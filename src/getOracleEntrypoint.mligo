#import "./checkerTypes.mligo" "CheckerT"
#include "./error.mligo"

(* index-tracking (in TOK/CHF) *)

[@inline] let get_oracle_entrypoint (external_contracts: CheckerT.external_contracts): ((nat * nat) contract) contract =
  match (Tezos.get_entrypoint_opt "%getPrice" external_contracts.oracle: ((nat * nat) contract) contract option) with
  | Some c -> c
  | None -> (failwith error_GetEntrypointOptFailureOracleEntrypoint: ((nat * nat) contract) contract)
