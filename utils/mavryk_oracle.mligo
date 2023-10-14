(* Simple example to use a Mavryk-style oracle ("satellite") with Checker *)
(* This code has not been audited by the Mavryk team and comes without any guarantee *)

#import "../math-lib-cameligo/lib.mligo" "Math"

type state = {
  satellite: address;
  owner: address;
}

type satellite_data =
[@layout:comb]
{
  round: nat;
  epoch: nat;
  data: nat;
  percentOracleResponse: nat;
  decimals: nat;
  lastUpdatedAt: timestamp;
}

let make_storage (satellite: address) (owner: address) = {
  satellite = satellite;
  owner = owner;
}

[@entry]
let main (satellite: address) (state: state): operation list * state =
  if Tezos.get_sender () = state.owner
  then [], { state with satellite = satellite }
  else failwith "unauthorized"

[@view]
let get_price () (state: state): (nat * nat) =
  match (Tezos.call_view "getlastCompletedData" () state.satellite: satellite_data option) with
    | None -> failwith "failed to retrieve data"
    (* Checker expects a price in tok, not in USD or EUR *)
    | Some data -> (Math.Core.power (10n, data.decimals), data.data)
