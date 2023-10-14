(* Just a plain FA2 contract in which we can make tokens out of thin air. *)
(* Mainly to be used in tests. *)

#import "./fa2Ledger.mligo" "FA2"
#import "./common.mligo" "Common"
#import "./tokenMetadata.mligo" "Tokens"

(* open Fa2Interface *)
(* open Fa2Ledger *)
(* open Common *)
(* open TokenMetadata *)

type mock_fa2_params =
  (* FA2 entrypoints *)
  | Balance_of of FA2.fa2_balance_of_param
  | Transfer of FA2.fa2_transfer list
  | Update_operators of FA2.fa2_update_operator list
  (* Contract-specific entrypoints *)
  | Mint of nat
  | Redeem of nat

type mock_fa2_state =
  { fa2_state : FA2.fa2_state;
    total_token : nat;
    metadata: (string, bytes) big_map;
  }

(** Make a fresh state. *)
let initial_mock_fa2 () =
  { fa2_state = FA2.initial_fa2_state;
    total_token = 0n;
    metadata = (Big_map.empty: (string, bytes) big_map);
  }

[@inline] let fa2_get_balance (st, owner, token_id: FA2.fa2_state * address * FA2.fa2_token_id): nat =
  let ledger = st.ledger in
  let key = (token_id, owner) in
  let () = if token_id = Tokens.mock_fa2_token_id then () else failwith "FA2_TOKEN_UNDEFINED" in
  FA2.get_fa2_ledger_value ledger key

[@inline] let fa2_run_balance_of (st, xs: FA2.fa2_state * FA2.fa2_balance_of_request list)
  : FA2.fa2_balance_of_response list =
  List.map
    (fun (req: FA2.fa2_balance_of_request) ->
       let { owner = owner; token_id = token_id; } : FA2.fa2_balance_of_request = req in
       let blnc = fa2_get_balance (st, owner, token_id) in
       { request=req; balance = blnc; }
    )
    xs

[@inline] let balance_of (state: mock_fa2_state) (param: FA2.fa2_balance_of_param) : operation list * mock_fa2_state =
  let { requests = requests; callback = callback; } = param in
  let response = fa2_run_balance_of (state.fa2_state, requests) in
  let op = Tezos.transaction response (0mutez) callback in
  ([op], state) (* unchanged state *)

[@inline] let fa2_run_transfer (state, xs: FA2.fa2_state * FA2.fa2_transfer list) : FA2.fa2_state * operation list =
  let state =
    (* Fold over FA2 Transfers *)
    List.fold_left
      (fun ((state, tx): (FA2.fa2_state * FA2.fa2_transfer)) ->
         let { from_ = from_; txs = txs; } = tx in
         (* Fold over the transactions in each FA2 Transfer *)
         List.fold_left
           (fun ((state, x): (FA2.fa2_state * FA2.fa2_transfer_destination)) ->
              let { to_ = to_; token_id = token_id; amount = amnt; } = x in

              if FA2.fa2_is_operator (state, (Tezos.get_sender ()), from_, token_id)
              then
                (* Update FA2 Ledger *)
                let () = if token_id = Tokens.mock_fa2_token_id then () else failwith "FA2_TOKEN_UNDEFINED" in
                let state = FA2.ledger_withdraw (state, token_id, from_, amnt) in
                let state = FA2.ledger_issue (state, token_id, to_, amnt) in
                state
              else
                (failwith "FA2_NOT_OPERATOR" : FA2.fa2_state)
           )
           state
           txs
      )
      state
      xs in
  (state, ([]: operation list))

[@inline] let transfer (state: mock_fa2_state) (xs: FA2.fa2_transfer list) : operation list * mock_fa2_state =
  let { fa2_state = fa2_state; total_token = total_token; metadata = metadata; } = state in (* deconstruct *)
  let fa2_state, ops = fa2_run_transfer (fa2_state, xs) in
  let state = { fa2_state = fa2_state; total_token = total_token; metadata = metadata; } in (* reconstruct *)
  (ops, state)

[@inline] let fa2_run_update_operators
    (st, xs: FA2.fa2_state * FA2.fa2_update_operator list) : FA2.fa2_state =
  List.fold_left
    (fun ((st : FA2.fa2_state), (x : FA2.fa2_update_operator)) ->
       match x with
       | Add_operator op ->
         let { owner = owner;
               operator = operator;
               token_id = token_id;
             } = op in
         (* The standard does not specify who is permitted to update operators. We restrict
            it only to the owner. *)
         if owner <> (Tezos.get_sender ())
         then (failwith "FA2_NOT_OWNER" : FA2.fa2_state)
         else
           { st  with
             operators =
               Big_map.add
                 (operator, owner, token_id)
                 ()
                 st.operators;
           }
       | Remove_operator op ->
         let { owner = owner;
               operator = operator;
               token_id = token_id;
             } = op in
         if owner <> (Tezos.get_sender ())
         then (failwith "FA2_NOT_OWNER" : FA2.fa2_state)
         else
           { st  with
             operators =
               Big_map.remove
                 (operator, owner, token_id)
                 st.operators;
           }
    )
    st
    xs

[@inline] let update_operators (state: mock_fa2_state) (xs: FA2.fa2_update_operator list) : operation list * mock_fa2_state =
  let { fa2_state = fa2_state; total_token = total_token; metadata = metadata; } = state in (* deconstruct *)
  let fa2_state = fa2_run_update_operators (fa2_state, xs) in
  let state = { fa2_state = fa2_state; total_token = total_token; metadata = metadata; } in (* reconstruct *)
  (([]: operation list), state)

[@inline] let mint (state: mock_fa2_state) (amnt: nat) : operation list * mock_fa2_state =
  let { fa2_state = fa2_state; total_token = total_token; metadata = metadata; } = state in (* deconstruct *)
  let fa2_state = FA2.ledger_issue (fa2_state, Tokens.mock_fa2_token_id, (Tezos.get_sender ()), amnt) in
  let total_token = total_token + amnt in
  let state = { fa2_state = fa2_state; total_token = total_token; metadata = metadata; } in (* reconstruct *)
  (([]: operation list), state)

[@inline] let redeem (state: mock_fa2_state) (amnt: nat) : operation list * mock_fa2_state =
  let { fa2_state = fa2_state; total_token = total_token; metadata = metadata; } = state in (* deconstruct *)
  let fa2_state = FA2.ledger_withdraw (fa2_state, Tokens.mock_fa2_token_id, (Tezos.get_sender ()), amnt) in
  let total_token =
    match is_nat (total_token - amnt) with
    | None -> (failwith "FA2_INSUFFICIENT_BALANCE" : nat)
    | Some tt -> tt in
  let state = { fa2_state = fa2_state; total_token = total_token; metadata = metadata; } in (* reconstruct *)
  (([]: operation list), state)

[@entry]
let main (op: mock_fa2_params) (state: mock_fa2_state): operation list * mock_fa2_state =
  let _ = Common.ensure_no_tez_given () in
  match op with
  (* FA2 entrypoints *)
  | Balance_of param -> balance_of state param
  | Transfer xs -> transfer state xs
  | Update_operators xs -> update_operators state xs
  (* Contract-specific entrypoints *)
  | Mint amnt -> mint state amnt
  | Redeem amnt -> redeem state amnt

[@view]
let view_get_balance ((owner, token_id): (address * FA2.fa2_token_id)) (state: mock_fa2_state) : nat =
  fa2_get_balance (state.fa2_state, owner, token_id)

[@view]
let view_total_supply (token_id: FA2.fa2_token_id) (state: mock_fa2_state) : nat =
  if token_id = Tokens.mock_fa2_token_id then
    state.total_token
  else
    failwith "FA2_TOKEN_UNDEFINED"

[@view]
let view_all_tokens (_unit: unit) (_state: mock_fa2_state) : FA2.fa2_token_id list =
  [ Tokens.mock_fa2_token_id ]

[@view]
let view_is_operator ((owner, (operator, token_id)): (address * (address * FA2.fa2_token_id))) (state: mock_fa2_state) : bool =
  FA2.fa2_is_operator (state.fa2_state, operator, owner, token_id)
