#import "./checkerTypes.mligo" "CheckerT"
#include "./error.mligo"

(* index-tracking (in TOK/CHF) *)

[@inline] let get_oracle_price (external_contracts: CheckerT.external_contracts): (nat * nat) =
  match (Tezos.call_view "get_price" () external_contracts.oracle: (nat * nat) option) with
  | Some r -> r
  | None -> failwith error_GetEntrypointOptFailureOracleView
