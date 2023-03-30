#import "./kit.mligo" "Kit"
#import "./lqt.mligo" "Lqt"
#import "./fa2Ledger.mligo" "Ledger"
#import "./fa2Interface.mligo" "FA2I"
#import "./tokenMetadata.mligo" "Tokens"

type fa2_transfer = FA2I.fa2_transfer
type fa2_token_id  = FA2I.fa2_token_id
type fa2_balance_of_param = FA2I.fa2_balance_of_param
type fa2_update_operator = FA2I.fa2_update_operator
type fa2_state = Ledger.fa2_state

[@inline] let fa2_all_tokens : nat list =
  [ Tokens.kit_token_id; Tokens.lqt_token_id ]

[@inline] let fa2_is_operator (st, operator, owner, token_id: fa2_state * address * address * fa2_token_id)  =
  Ledger.fa2_is_operator (st, operator, owner, token_id)

[@inline] let ensure_valid_fa2_token (n: Ledger.fa2_token_id): unit =
  if n = Tokens.kit_token_id || n = Tokens.lqt_token_id
  then ()
  else failwith "FA2_TOKEN_UNDEFINED"

[@inline] let ledger_issue_kit
    (st, addr, amnt: Ledger.fa2_state * address * Kit.kit) : Ledger.fa2_state =
  Ledger.ledger_issue (st, Tokens.kit_token_id, addr, Kit.kit_to_denomination_nat amnt)

[@inline] let ledger_withdraw_kit
    (st, addr, amnt: Ledger.fa2_state * address * Kit.kit) : Ledger.fa2_state =
  Ledger.ledger_withdraw (st, Tokens.kit_token_id, addr, Kit.kit_to_denomination_nat amnt)

[@inline] let ledger_issue_then_withdraw_kit
    (st, addr, amnt_to_issue, amnt_to_withdraw: Ledger.fa2_state * address * Kit.kit * Kit.kit) : Ledger.fa2_state =
  Ledger.ledger_issue_then_withdraw (st, Tokens.kit_token_id, addr, Kit.kit_to_denomination_nat amnt_to_issue, Kit.kit_to_denomination_nat amnt_to_withdraw)

[@inline] let ledger_issue_lqt
    (st, addr, amnt: Ledger.fa2_state * address * Lqt.lqt) : Ledger.fa2_state =
  Ledger.ledger_issue (st, Tokens.lqt_token_id, addr, Lqt.lqt_to_denomination_nat amnt)

[@inline] let ledger_withdraw_lqt
    (st, addr, amnt: Ledger.fa2_state * address * Lqt.lqt) : Ledger.fa2_state =
  Ledger.ledger_withdraw (st, Tokens.lqt_token_id, addr, Lqt.lqt_to_denomination_nat amnt)

[@inline] let fa2_get_balance (st, owner, token_id: Ledger.fa2_state * address * Ledger.fa2_token_id): nat =
  let ledger = st.ledger in
  let key = (token_id, owner) in
  let () = ensure_valid_fa2_token token_id in
  Ledger.get_fa2_ledger_value ledger key

[@inline] let fa2_run_balance_of (st, xs: Ledger.fa2_state * Ledger.fa2_balance_of_request list)
  : Ledger.fa2_balance_of_response list =
  List.map
    (fun (req: Ledger.fa2_balance_of_request) ->
       let { owner = owner; token_id = token_id; } : Ledger.fa2_balance_of_request = req in
       let blnc = fa2_get_balance (st, owner, token_id) in
       { request=req; balance = blnc; }
    )
    xs

[@inline] let fa2_run_update_operators
    (st, xs: Ledger.fa2_state * Ledger.fa2_update_operator list) : Ledger.fa2_state =
  List.fold_left
    (fun ((st : Ledger.fa2_state), (x : Ledger.fa2_update_operator)) ->
       match x with
       | Add_operator op ->
         let { owner = owner;
               operator = operator;
               token_id = token_id;
             } = op in
         (* The standard does not specify who is permitted to update operators. We restrict
            it only to the owner. *)
         if owner <> Tezos.get_sender ()
         then (failwith "FA2_NOT_OWNER" : Ledger.fa2_state)
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
         if owner <> Tezos.get_sender ()
         then (failwith "FA2_NOT_OWNER" : Ledger.fa2_state)
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

[@inline] let fa2_run_transfer
    (st, xs: Ledger.fa2_state * Ledger.fa2_transfer list) : Ledger.fa2_state =
  List.fold_left
    (fun ((st, tx): Ledger.fa2_state * Ledger.fa2_transfer) ->
       let { from_ = from_; txs = txs; } = tx in

       List.fold_left
         (fun ((st, x): Ledger.fa2_state * Ledger.fa2_transfer_destination) ->
            let { to_ = to_; token_id = token_id; amount = amnt; } = x in
            if fa2_is_operator (st, Tezos.get_sender (), from_, token_id)
            then
              let () = ensure_valid_fa2_token token_id in
              let st = Ledger.ledger_withdraw (st, token_id, from_, amnt) in
              let st = Ledger.ledger_issue (st, token_id, to_, amnt) in
              st
            else
              (failwith "FA2_NOT_OPERATOR" : Ledger.fa2_state)
         )
         st
         txs
    )
    st
    xs

(* BEGIN_OCAML
(* [@@@coverage off] *)

let fa2_get_token_balance (st: fa2_state) (token_id: fa2_token_id): nat =
  Big_map.bindings st.ledger
  |> List.filter (fun ((id, _owner), _amnt) -> id = token_id)
  |> List.map (fun ((_id, _owner), amnt) -> amnt)
  |> List.fold_left (fun x y -> add_nat_nat x y) (0n)

let fa2_get_total_kit_balance (st: fa2_state) : kit = kit_of_denomination (fa2_get_token_balance st kit_token_id)
let fa2_get_total_lqt_balance (st: fa2_state) : lqt = lqt_of_denomination (fa2_get_token_balance st lqt_token_id)

let get_kit_credits_from_fa2_state (st: fa2_state) : ((address * nat) list) =
  (* Note: for now let's just focus on the kit on the ledger. *)
  let kit_map =
    Big_map.bindings st.ledger
    |> List.filter (fun ((id, _owner), _amnt) -> id = kit_token_id)
    |> List.map (fun ((_id, owner), amnt) -> (owner, amnt))
    |> List.stable_sort compare
  in
  kit_map

(* [@@@coverage on] *)
   END_OCAML *)
