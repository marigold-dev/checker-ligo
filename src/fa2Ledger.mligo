(* open Fa2Interface *)

(*
Reference:
https://gitlab.com/tzip/tzip/-/blob/4b3c67aad5abbf04ec36caea4a1809e7b6e55bb8/proposals/tzip-12/tzip-12.md
*)

type fa2_state =
  { ledger : (fa2_token_id * address, nat) big_map;
    operators : ( address (* operator *)
                  * address (* owner *)
                  * fa2_token_id
                , unit
                ) big_map;
  }

[@inline] let initial_fa2_state =
  { ledger = (Big_map.empty: (fa2_token_id * address, nat) big_map);
    operators = (Big_map.empty: (address * address * fa2_token_id, unit) big_map);
  }

[@inline] let get_fa2_ledger_value
    (ledger: (fa2_token_id * address, nat) big_map)
    (key: fa2_token_id * address)
  : nat =
  match Big_map.find_opt key ledger with
  | Some i -> i
  | None -> 0n

[@inline] let set_fa2_ledger_value
    (ledger: (fa2_token_id * address, nat) big_map)
    (key: fa2_token_id * address)
    (value: nat)
  : (fa2_token_id * address, nat) big_map =
  if value = 0n
  then Big_map.remove key ledger
  else Big_map.add key value ledger

[@inline] let ledger_issue
    (st, tok, addr, amnt: fa2_state * fa2_token_id * address * nat) : fa2_state =
  let ledger = st.ledger in
  let key = (tok , addr) in
  let prev_balance = get_fa2_ledger_value ledger key in
  let new_balance = add_nat_nat prev_balance amnt in
  let ledger = set_fa2_ledger_value ledger key new_balance in
  { st with ledger = ledger }

[@inline] let ledger_withdraw
    (st, tok, addr, amnt: fa2_state * fa2_token_id * address * nat) : fa2_state =
  let ledger = st.ledger in
  let key = (tok, addr) in
  let prev_balance = get_fa2_ledger_value ledger key in
  let new_balance =
    match is_nat (sub_nat_nat prev_balance amnt) with
    | None -> (failwith "FA2_INSUFFICIENT_BALANCE" : nat)
    | Some b -> b in
  let ledger = set_fa2_ledger_value ledger key new_balance in
  { st with ledger = ledger }

(* NOTE: Checker-specific, this one. Needed to save on gas costs. *)
[@inline] let ledger_issue_then_withdraw
    (st, tok, addr, amnt_to_issue, amnt_to_withdraw: fa2_state * fa2_token_id * address * nat * nat) : fa2_state =
  let ledger = st.ledger in
  let key = (tok , addr) in
  let balance_ = get_fa2_ledger_value ledger key in
  (* ISSUE *)
  let balance_ = add_nat_nat balance_ amnt_to_issue in
  (* WITHDRAW *)
  let balance_ =
    match is_nat (sub_nat_nat balance_ amnt_to_withdraw) with
    | None -> (failwith "FA2_INSUFFICIENT_BALANCE" : nat)
    | Some b -> b in
  (* UPDATE STATE *)
  let ledger = set_fa2_ledger_value ledger key balance_ in
  { st with ledger = ledger }

[@inline] let fa2_is_operator (st, operator, owner, token_id: fa2_state * address * address * fa2_token_id) =
  owner = operator || Big_map.mem (operator, owner, token_id) st.operators

