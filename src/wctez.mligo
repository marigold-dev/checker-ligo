(* FA2 wrapper for ctez to allow it to integrate with Checker *)
#import "./fa2Ledger.mligo" "FA2"
#import "./fa12Interface.mligo" "FA12"
#import "./common.mligo" "Common"
#import "./tokenMetadata.mligo" "Tokens"
#include "./error.mligo"

(*****************************************************************************)
(**                          {1 WRAPPER TYPES}                               *)
(*****************************************************************************)

type wctez_state =
  { fa2_state : FA2.fa2_state;
    total_token : nat;
    ctez_fa12_address : address;
    metadata: (string, bytes) big_map;
  }

(** Make a fresh state. *)
let initial_wctez (ctez_fa12_address: address) =
  { fa2_state = FA2.initial_fa2_state;
    total_token = 0n;
    ctez_fa12_address = ctez_fa12_address;
    metadata = (Big_map.empty: (string, bytes) big_map);
  }

type wctez_params =
  (* FA2 entrypoints *)
  | Balance_of of FA2.fa2_balance_of_param
  | Transfer of FA2.fa2_transfer list
  | Update_operators of FA2.fa2_update_operator list
  (* Wrapper-specific entrypoints *)
  | Mint of nat
  | Redeem of nat

(*****************************************************************************)
(**                             {1 LEDGER}                                   *)
(*****************************************************************************)

[@inline] let ledger_issue_wctez_token
    (st, addr, amnt: FA2.fa2_state * address * nat) : FA2.fa2_state =
  FA2.ledger_issue (st, Tokens.wctez_token_id, addr, amnt)

[@inline] let ledger_withdraw_wctez_token
    (st, addr, amnt: FA2.fa2_state * address * nat) : FA2.fa2_state =
  FA2.ledger_withdraw (st, Tokens.wctez_token_id, addr, amnt)

(*****************************************************************************)
(**                        {1 FA2 ENTRYPOINTS}                               *)
(*****************************************************************************)

[@inline] let fa2_get_balance (st, owner, token_id: FA2.fa2_state * address * FA2.fa2_token_id): nat =
  let ledger = st.ledger in
  let key = (token_id, owner) in
  let () = if token_id = Tokens.wctez_token_id then () else failwith "FA2_TOKEN_UNDEFINED" in
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

[@inline] let balance_of (state: wctez_state) (param: FA2.fa2_balance_of_param) : operation list * wctez_state =
  let _ = Common.ensure_no_tez_given () in
  let { requests = requests; callback = callback; } = param in
  let response = fa2_run_balance_of (state.fa2_state, requests) in
  let op = Tezos.transaction response (0mutez) callback in
  ([op], state) (* unchanged state *)


[@inline] let fa2_run_transfer (initial_state, xs: wctez_state * FA2.fa2_transfer list) : wctez_state * operation list =
  let state =
    (* Fold over FA2 Transfers *)
    List.fold_left
      (fun ((st, tx): (wctez_state * FA2.fa2_transfer)) ->
         let { from_ = from_; txs = txs; } = tx in
         (* Fold over the transactions in each FA2 Transfer *)
         List.fold_left
           (fun ((st, x): (wctez_state * FA2.fa2_transfer_destination)) ->
              let { fa2_state = fa2_state; total_token = total_token; ctez_fa12_address = ctez_fa12_address; metadata = metadata; } = st in (* deconstruct *)
              let { to_ = to_; token_id = token_id; amount = amnt; } = x in

              if FA2.fa2_is_operator (fa2_state, (Tezos.get_sender ()), from_, token_id)
              then
                (* Update FA2 Ledger *)
                let () = if token_id = Tokens.wctez_token_id then () else failwith "FA2_TOKEN_UNDEFINED" in
                let fa2_state = FA2.ledger_withdraw (fa2_state, token_id, from_, amnt) in
                let fa2_state = FA2.ledger_issue (fa2_state, token_id, to_, amnt) in
                let st = { fa2_state = fa2_state; total_token = total_token; ctez_fa12_address = ctez_fa12_address; metadata = metadata; } in (* reconstruct *)
                st
              else
                (failwith "FA2_NOT_OPERATOR" : wctez_state)
           )
           st
           txs
      )
      initial_state
      xs in
  (state, ([]: operation list))

[@inline] let transfer (state: wctez_state) (xs: FA2.fa2_transfer list) : operation list * wctez_state =
  let _ = Common.ensure_no_tez_given () in
  let state, ops = fa2_run_transfer (state, xs) in
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

[@inline] let update_operators (state: wctez_state) (xs: FA2.fa2_update_operator list) : operation list * wctez_state =
  let _ = Common.ensure_no_tez_given () in
  let { fa2_state = fa2_state; total_token = total_token; ctez_fa12_address = ctez_fa12_address; metadata = metadata; } = state in (* deconstruct *)
  let fa2_state = fa2_run_update_operators (fa2_state, xs) in
  let state = { fa2_state = fa2_state; total_token = total_token; ctez_fa12_address = ctez_fa12_address; metadata = metadata; } in (* reconstruct *)
  (([]: operation list), state)

(*****************************************************************************)
(**                      {1 WRAPPER ENTRYPOINTS}                             *)
(*****************************************************************************)

[@inline] let mint (state: wctez_state) (amnt: nat) : operation list * wctez_state =
  let { fa2_state = fa2_state; total_token = total_token; ctez_fa12_address = ctez_fa12_address; metadata = metadata; } = state in (* deconstruct *)
  (* Emit an operation to ctez transfering amnt of the caller's ctez to this contract *)
  let ctez_fa12_contract = match (Tezos.get_entrypoint_opt "%transfer" ctez_fa12_address : FA12.fa12_transfer contract option) with
    | Some c -> c
    | None -> (failwith error_GetEntrypointOptFailureFA12Transfer : FA12.fa12_transfer contract)
  in
  let op = Tezos.transaction
      {address_from = (Tezos.get_sender ()); address_to = (Tezos.get_self_address ()); value = amnt;}
      (0mutez)
      ctez_fa12_contract
  in
  (* Issue the specified amount of tokens to the caller *)
  let fa2_state = ledger_issue_wctez_token (fa2_state, (Tezos.get_sender ()), amnt) in
  let total_token = total_token + amnt in
  let state = { fa2_state = fa2_state; total_token = total_token; ctez_fa12_address = ctez_fa12_address; metadata = metadata; } in (* reconstruct *)
  ([op], state)

[@inline] let redeem (state: wctez_state) (amnt: nat) : operation list * wctez_state =
  let { fa2_state = fa2_state; total_token = total_token; ctez_fa12_address = ctez_fa12_address; metadata = metadata; } = state in (* deconstruct *)
  (* Emit an operation to ctez transfering amnt of the caller's ctez to this contract *)
  let ctez_fa12_contract = match (Tezos.get_entrypoint_opt "%transfer" ctez_fa12_address : FA12.fa12_transfer contract option) with
    | Some c -> c
    | None -> (failwith error_GetEntrypointOptFailureFA12Transfer : FA12.fa12_transfer contract)
  in
  let op = Tezos.transaction
      {address_from = (Tezos.get_self_address ()); address_to = (Tezos.get_sender ()); value = amnt;}
      (0mutez)
      ctez_fa12_contract
  in
  (* Remove the specified amount of tokens for the caller *)
  let fa2_state = ledger_withdraw_wctez_token (fa2_state, (Tezos.get_sender ()), amnt) in
  (* Remove the specified amount of token from circulation *)
  let total_token =
    match is_nat (total_token - amnt) with
    | None -> (failwith "FA2_INSUFFICIENT_BALANCE" : nat)
    | Some tt -> tt in
  let state = { fa2_state = fa2_state; total_token = total_token; ctez_fa12_address = ctez_fa12_address; metadata = metadata; } in (* reconstruct *)
  ([op], state)

(*****************************************************************************)
(**                              {1 MAIN}                                    *)
(*****************************************************************************)

[@entry]
let main (op: wctez_params) (state: wctez_state): operation list * wctez_state =
  match op with
  (* FA2 entrypoints *)
  | Balance_of param -> balance_of state param
  | Transfer xs -> transfer state xs
  | Update_operators xs -> update_operators state xs
  (* Wrapper-specific entrypoints *)
  | Mint amnt -> mint state amnt
  | Redeem amnt -> redeem state amnt

(*****************************************************************************)
(**                       {1 OFFLINE FA2 VIEWS}                              *)
(*****************************************************************************)

let view_get_balance ((owner, token_id), state: (address * FA2.fa2_token_id) * wctez_state) : nat =
  fa2_get_balance (state.fa2_state, owner, token_id)

let view_total_supply (token_id, state: FA2.fa2_token_id * wctez_state) : nat =
  if token_id = Tokens.wctez_token_id then
    state.total_token
  else
    failwith "FA2_TOKEN_UNDEFINED"

let view_all_tokens ((), _state: unit * wctez_state) : FA2.fa2_token_id list =
  [ Tokens.wctez_token_id ]

let view_is_operator ((owner, (operator, token_id)), state: (address * (address * FA2.fa2_token_id)) * wctez_state) : bool =
  FA2.fa2_is_operator (state.fa2_state, operator, owner, token_id)
