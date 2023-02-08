(* open Fa2Interface *)
(* open Fa2Ledger *)
(* open Common *)
(* open VaultTypes *)
(* open Error *)
(* open TokenMetadata *)

(** Originate a vault contract with no delegate and zero tez. This way we can
  * originate vaults pretty easily, everytime we look one up: if it's not
  * there, just originate it now. *)
[@inline] let originate_vault (owner: address) : operation * address =
  Tezos.create_contract
    (fun (p, storage : vault_parameter * vault_storage) ->
       match p with
       | Vault_set_delegate kho ->
         if (Tezos.get_amount ()) <> 0mutez then
           (failwith ((-1)) : operation list * vault_storage) (* unwanted tez *)
         else if (Tezos.get_sender ()) <> storage.owner then
           (failwith ((-2)) : operation list * vault_storage) (* unauthorized *)
         else
           ([Tezos.set_delegate kho], storage)
       | Vault_receive_tez () ->
         (* NOTE: allowed from everyone. *)
         (([]: operation list), storage)
       | Vault_send_tez_to_vault tz_recipient ->
         if (Tezos.get_amount ()) <> 0mutez then
           (failwith ((-3)) : operation list * vault_storage) (* unwanted tez *)
         else if (Tezos.get_sender ()) <> storage.owner then
           (failwith ((-4)) : operation list * vault_storage) (* unauthorized *)
         else
           let tz, recipient = tz_recipient in
           let op = match (Tezos.get_entrypoint_opt "%vault_receive_tez" recipient : unit contract option) with
             | Some c -> Tezos.transaction () tz c
             | None -> (failwith ((-8)) : operation) in
           ([op], storage)
       | Vault_send_tez_to_contract tz_recipient ->
         if (Tezos.get_amount ()) <> 0mutez then
           (failwith ((-5)) : operation list * vault_storage) (* unwanted tez *)
         else if (Tezos.get_sender ()) <> storage.owner then
           (failwith ((-6)) : operation list * vault_storage) (* unauthorized *)
         else
           let tz, recipient = tz_recipient in
           let op = match (Tezos.get_contract_opt recipient : unit contract option) with
             | Some c -> Tezos.transaction () tz c
             | None -> (failwith ((-7)) : operation) in
           ([op], storage)
    )
    (None: key_hash option)
    (0mutez)
    {owner = owner}

(*****************************************************************************)
(**                          {1 WRAPPER TYPES}                               *)
(*****************************************************************************)

(** Map from vault owner addresses to vault addresses. *)
type vault_map = (address, address) big_map

type wtez_state =
  { fa2_state : fa2_state;
    total_token : nat;
    vaults : vault_map;
    metadata: (string, bytes) big_map;
  }

(** Make a fresh state. *)
let initial_wtez () =
  { fa2_state = initial_fa2_state;
    total_token = 0n;
    vaults = (Big_map.empty: (address, address) big_map);
    metadata = (Big_map.empty: (string, bytes) big_map);
  }

type wtez_params =
  (* FA2 entrypoints *)
  | Balance_of of fa2_balance_of_param
  | Transfer of fa2_transfer list
  | Update_operators of fa2_update_operator list
  (* Wrapper-specific entrypoints *)
  | Deposit of unit (* TODO: not nice, having a unit type. Perhaps pass the tez as a number too? *)
  | Withdraw of tez
  | Set_delegate of (key_hash option)
  (* Internal entrypoints *)
  | Call_vault_receive_tez of (address * tez)
  | Call_vault_send_tez_to_contract of (address * tez * address)
  | Call_vault_send_tez_to_vault of (address * tez * address)
  | Call_vault_set_delegate of (address * key_hash option)

type vault_found = VaultFound | VaultNotFound

(** Find the address of the vault of given user, or originate it on the fly,
  * with (Tezos.get_self_address ()) as the owner. *)
[@inline] let find_vault_address_append (vaults: vault_map) (user: address) (ops: operation list) : vault_found * operation list * vault_map * address =
  match Big_map.find_opt user vaults with
  | Some vault_address -> VaultFound, ops, vaults, vault_address
  | None ->
    let op, vault_address = originate_vault (Tezos.get_self_address ()) in
    VaultNotFound, (op :: ops), (Big_map.update user (Some vault_address) vaults), vault_address

(*****************************************************************************)
(**                             {1 LEDGER}                                   *)
(*****************************************************************************)

[@inline] let ledger_issue_tez_token
    (st, addr, amnt: fa2_state * address * tez) : fa2_state =
  ledger_issue (st, wtez_token_id, addr, tez_to_mutez_nat amnt)

[@inline] let ledger_withdraw_tez_token
    (st, addr, amnt: fa2_state * address * tez) : fa2_state =
  ledger_withdraw (st, wtez_token_id, addr, tez_to_mutez_nat amnt)

(*****************************************************************************)
(**                        {1 FA2 ENTRYPOINTS}                               *)
(*****************************************************************************)

[@inline] let fa2_get_balance (st, owner, token_id: fa2_state * address * fa2_token_id): nat =
  let ledger = st.ledger in
  let key = (token_id, owner) in
  let () = if token_id = wtez_token_id then () else failwith "FA2_TOKEN_UNDEFINED" in
  get_fa2_ledger_value ledger key

[@inline] let fa2_run_balance_of (st, xs: fa2_state * fa2_balance_of_request list)
  : fa2_balance_of_response list =
  List.map
    (fun (req: fa2_balance_of_request) ->
       let { owner = owner; token_id = token_id; } : fa2_balance_of_request = req in
       let blnc = fa2_get_balance (st, owner, token_id) in
       { request=req; balance = blnc; }
    )
    xs

[@inline] let balance_of (state: wtez_state) (param: fa2_balance_of_param) : operation list * wtez_state =
  let _ = ensure_no_tez_given () in
  let { requests = requests; callback = callback; } = param in
  let response = fa2_run_balance_of (state.fa2_state, requests) in
  let op = Tezos.transaction response (0mutez) callback in
  ([op], state) (* unchanged state; no attempt to originate vaults either *)

[@inline] let reverse_op_list (ops: operation list) : operation list =
  List.fold_left
    (fun ((ops, op) : operation list * operation) -> (op :: ops))
    ([] : operation list)
    ops

[@inline] let fa2_run_transfer (st, xs: wtez_state * fa2_transfer list) : wtez_state * operation list =
  let state, rev_ops =
    List.fold_left
      (fun (((st, ops), tx): (wtez_state * operation list) * fa2_transfer) ->
         let { fa2_state = fa2_state; total_token = total_token; vaults = vaults; metadata = metadata; } = st in (* deconstruct *)
         let { from_ = from_; txs = txs; } = tx in

         (* Origination of the from_ vault, if needed *)
         let from_vault_found, ops, vaults, from_vault_address = find_vault_address_append vaults from_ ops in
         let st = { fa2_state = fa2_state; total_token = total_token; vaults = vaults; metadata = metadata; } in (* reconstruct *)

         List.fold_left
           (fun (((st, ops), x): (wtez_state * operation list) * fa2_transfer_destination) ->
              let { fa2_state = fa2_state; total_token = total_token; vaults = vaults; metadata = metadata; } = st in (* deconstruct *)
              let { to_ = to_; token_id = token_id; amount = amnt; } = x in

              if fa2_is_operator (fa2_state, (Tezos.get_sender ()), from_, token_id)
              then
                (* FA2-related changes *)
                let () = if token_id = wtez_token_id then () else failwith "FA2_TOKEN_UNDEFINED" in
                let fa2_state = ledger_withdraw (fa2_state, token_id, from_, amnt) in
                let fa2_state = ledger_issue (fa2_state, token_id, to_, amnt) in
                (* Origination of the to_ vault, if needed *)
                let _to_vault_found, ops, vaults, to_vault_address = find_vault_address_append vaults to_ ops in
                (* Instruct the from_ vault to send the actual tez to the to_ vault *)
                let op = match from_vault_found with
                  | VaultFound -> begin
                      (* Case 1: the vault for from_ exists already; make a direct call *)
                      match (Tezos.get_entrypoint_opt "%vault_send_tez_to_vault" from_vault_address : (tez * address) contract option) with
                      | Some c -> Tezos.transaction (tez_of_mutez_nat amnt, to_vault_address) (0mutez) c
                      | None -> (failwith error_GetEntrypointOptFailureVaultSendTezToVault : operation)
                    end
                  | VaultNotFound -> begin
                      (* Case 2: the vault for from_ does not exist already; make an indirect call (more expensive) *)
                      match (Tezos.get_entrypoint_opt "%call_vault_send_tez_to_vault" (Tezos.get_self_address ()) : (address * tez * address) contract option) with
                      | Some c -> Tezos.transaction (from_vault_address, tez_of_mutez_nat amnt, to_vault_address) (0mutez) c
                      | None -> (failwith error_GetEntrypointOptFailureCallVaultSendTezToVault : operation)
                    end in
                let ops = (op :: ops) in

                let st = { fa2_state = fa2_state; total_token = total_token; vaults = vaults; metadata = metadata; } in (* reconstruct *)
                (st, ops)
              else
                (failwith "FA2_NOT_OPERATOR" : wtez_state * operation list)
           )
           (st, ops)
           txs
      )
      (st, ([]: operation list))
      xs in
  (state, reverse_op_list rev_ops)

[@inline] let transfer (state: wtez_state) (xs: fa2_transfer list) : operation list * wtez_state =
  let _ = ensure_no_tez_given () in
  let state, ops = fa2_run_transfer (state, xs) in
  (ops, state)

[@inline] let fa2_run_update_operators
    (st, xs: fa2_state * fa2_update_operator list) : fa2_state =
  List.fold_left
    (fun ((st : fa2_state), (x : fa2_update_operator)) ->
       match x with
       | Add_operator op ->
         let { owner = owner;
               operator = operator;
               token_id = token_id;
             } = op in
         (* The standard does not specify who is permitted to update operators. We restrict
            it only to the owner. *)
         if owner <> (Tezos.get_sender ())
         then (failwith "FA2_NOT_OWNER" : fa2_state)
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
         then (failwith "FA2_NOT_OWNER" : fa2_state)
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

[@inline] let update_operators (state: wtez_state) (xs: fa2_update_operator list) : operation list * wtez_state =
  let _ = ensure_no_tez_given () in
  let { fa2_state = fa2_state; total_token = total_token; vaults = vaults; metadata = metadata; } = state in (* deconstruct *)
  let fa2_state = fa2_run_update_operators (fa2_state, xs) in
  let state = { fa2_state = fa2_state; total_token = total_token; vaults = vaults; metadata = metadata; } in (* reconstruct *)
  (([]: operation list), state) (* no need to originate vaults *)

(*****************************************************************************)
(**                      {1 WRAPPER ENTRYPOINTS}                             *)
(*****************************************************************************)

[@inline] let deposit (state: wtez_state) (_: unit) : operation list * wtez_state =
  let { fa2_state = fa2_state; total_token = total_token; vaults = vaults; metadata = metadata; } = state in (* deconstruct *)
  let fa2_state = ledger_issue_tez_token (fa2_state, (Tezos.get_sender ()), (Tezos.get_amount ())) in
  let total_token = add_nat_nat total_token (tez_to_mutez_nat (Tezos.get_amount ())) in
  match Big_map.find_opt (Tezos.get_sender ()) vaults with
  | Some vault_address ->
    (* Case 1: The vault already exists. We can just deposit the tez into it
     * via a direct call. Cheaper than Case 2 below. *)
    let op = match (Tezos.get_entrypoint_opt "%vault_receive_tez" vault_address : unit contract option) with
      | Some c -> Tezos.transaction () (Tezos.get_amount ()) c (* !!! *)
      | None -> (failwith error_GetEntrypointOptFailureVaultReceiveTez : operation) in
    let state = { fa2_state = fa2_state; total_token = total_token; vaults = vaults; metadata = metadata; } in (* reconstruct *)
    ([op], state)
  | None ->
    (* Case 2: The vault does not exist yet. We need to create it, and then
     * deposit the tez into it via an indirect call. This can be expensive the
     * first time. *)
    let origination, vault_address = originate_vault (Tezos.get_self_address ()) in
    let vaults = Big_map.update (Tezos.get_sender ()) (Some vault_address) vaults in
    let state = { fa2_state = fa2_state; total_token = total_token; vaults = vaults; metadata = metadata; } in (* reconstruct *)
    let op = match (Tezos.get_entrypoint_opt "%call_vault_receive_tez" (Tezos.get_self_address ()) : (address * tez) contract option) with
      | Some c -> Tezos.transaction (vault_address, (Tezos.get_amount ())) (0mutez) c (* !!! *)
      | None -> (failwith error_GetEntrypointOptFailureCallVaultReceiveTez : operation) in
    ([origination; op], state)

[@inline] let withdraw (state: wtez_state) (amnt: tez) : operation list * wtez_state =
  let { fa2_state = fa2_state; total_token = total_token; vaults = vaults; metadata = metadata; } = state in (* deconstruct *)
  let _ = ensure_no_tez_given () in
  let fa2_state = ledger_withdraw_tez_token (fa2_state, (Tezos.get_sender ()), amnt) in
  let total_token =
    match is_nat (sub_nat_nat total_token (tez_to_mutez_nat amnt)) with
    | None -> (failwith "FA2_INSUFFICIENT_BALANCE" : nat)
    | Some tt -> tt in
  match Big_map.find_opt (Tezos.get_sender ()) vaults with
  | Some vault_address ->
    (* Case 1: The vault already exists. We can just instruct it to send the
     * actual tez to the owner via a direct call. Cheaper than Case 2 below. *)
    let op = match (Tezos.get_entrypoint_opt "%vault_send_tez_to_contract" vault_address : (tez * address) contract option) with
      | Some c -> Tezos.transaction (amnt, (Tezos.get_sender ())) (0mutez) c
      | None -> (failwith error_GetEntrypointOptFailureVaultSendTezToContract : operation) in
    let state = { fa2_state = fa2_state; total_token = total_token; vaults = vaults; metadata = metadata; } in (* reconstruct *)
    ([op], state)
  | None ->
    (* Case 2: The vault does not exist yet. We need to create it, and then
     * instruct it to send the actual tez to the owner via an indirect call.
     * This can be expensive the first time. *)
    let origination, vault_address = originate_vault (Tezos.get_self_address ()) in
    let vaults = Big_map.update (Tezos.get_sender ()) (Some vault_address) vaults in
    let state = { fa2_state = fa2_state; total_token = total_token; vaults = vaults; metadata = metadata; } in (* reconstruct *)
    let op = match (Tezos.get_entrypoint_opt "%call_vault_send_tez_to_contract" (Tezos.get_self_address ()) : (address * tez * address) contract option) with
      | Some c -> Tezos.transaction (vault_address, amnt, (Tezos.get_sender ())) (0mutez) c
      | None -> (failwith error_GetEntrypointOptFailureCallVaultSendTezToContract : operation) in
    ([origination; op], state)

[@inline] let set_delegate (state: wtez_state) (kho: key_hash option) : operation list * wtez_state =
  let { fa2_state = fa2_state; total_token = total_token; vaults = vaults; metadata = metadata; } = state in (* deconstruct *)
  let _ = ensure_no_tez_given () in
  match Big_map.find_opt (Tezos.get_sender ()) vaults with
  | Some vault_address ->
    (* Case 1: The vault already exists. We can just instruct it to set its own
     * delegate via a direct call. Cheaper than Case 2 below. *)
    let op = match (Tezos.get_entrypoint_opt "%vault_set_delegate" vault_address : key_hash option contract option) with
      | Some c -> Tezos.transaction kho (0mutez) c
      | None -> (failwith error_GetEntrypointOptFailureVaultSetDelegate : operation) in
    ([op], state)
  | None ->
    (* Case 2: The vault does not exist yet. We need to create it, and then
     * instruct it to set its own delegate via an indirect call. This can be
     * expensive the first time. *)
    let origination, vault_address = originate_vault (Tezos.get_self_address ()) in
    let vaults = Big_map.update (Tezos.get_sender ()) (Some vault_address) vaults in
    let state = { fa2_state = fa2_state; total_token = total_token; vaults = vaults; metadata = metadata; } in (* reconstruct *)
    let op = match (Tezos.get_entrypoint_opt "%call_vault_set_delegate" (Tezos.get_self_address ()) : (address * key_hash option) contract option) with
      | Some c -> Tezos.transaction (vault_address, kho) (0mutez) c
      | None -> (failwith error_GetEntrypointOptFailureCallVaultSetDelegate : operation) in
    ([origination; op], state)

(*****************************************************************************)
(**                      {1 INTERNAL ENTRYPOINTS}                            *)
(*****************************************************************************)

[@inline] let call_vault_receive_tez (state: wtez_state) (vault_address, amnt : address * tez) : operation list * wtez_state =
  if (Tezos.get_sender ()) <> (Tezos.get_self_address ()) then
    (failwith error_UnauthorisedCaller : operation list * wtez_state)
  else
    let op = match (Tezos.get_entrypoint_opt "%vault_receive_tez" vault_address : unit contract option) with
      | Some c -> Tezos.transaction () amnt c
      | None -> (failwith error_GetEntrypointOptFailureVaultReceiveTez : operation) in
    ([op], state)

[@inline] let call_vault_send_tez_to_contract (state: wtez_state) (vault_address, amnt, recipient : address * tez * address) : operation list * wtez_state =
  if (Tezos.get_sender ()) <> (Tezos.get_self_address ()) then
    (failwith error_UnauthorisedCaller : operation list * wtez_state)
  else
    let op = match (Tezos.get_entrypoint_opt "%vault_send_tez_to_contract" vault_address : (tez * address) contract option) with
      | Some c -> Tezos.transaction (amnt, recipient) (0mutez) c
      | None -> (failwith error_GetEntrypointOptFailureVaultSendTezToContract : operation) in
    ([op], state)

[@inline] let call_vault_send_tez_to_vault (state: wtez_state) (vault_address, amnt, recipient : address * tez * address) : operation list * wtez_state =
  if (Tezos.get_sender ()) <> (Tezos.get_self_address ()) then
    (failwith error_UnauthorisedCaller : operation list * wtez_state)
  else
    let op = match (Tezos.get_entrypoint_opt "%vault_send_tez_to_vault" vault_address : (tez * address) contract option) with
      | Some c -> Tezos.transaction (amnt, recipient) (0mutez) c
      | None -> (failwith error_GetEntrypointOptFailureVaultSendTezToVault : operation) in
    ([op], state)

[@inline] let call_vault_set_delegate (state: wtez_state) (vault_address, kho : address * key_hash option) : operation list * wtez_state =
  if (Tezos.get_sender ()) <> (Tezos.get_self_address ()) then
    (failwith error_UnauthorisedCaller : operation list * wtez_state)
  else
    let op = match (Tezos.get_entrypoint_opt "%vault_set_delegate" vault_address : key_hash option contract option) with
      | Some c -> Tezos.transaction kho (0mutez) c
      | None -> (failwith error_GetEntrypointOptFailureVaultSetDelegate : operation) in
    ([op], state)

(*****************************************************************************)
(**                              {1 MAIN}                                    *)
(*****************************************************************************)

let main (op, state: wtez_params * wtez_state): operation list * wtez_state =
  match op with
  (* FA2 entrypoints *)
  | Balance_of param -> balance_of state param
  | Transfer xs -> transfer state xs
  | Update_operators xs -> update_operators state xs
  (* Wrapper-specific entrypoints *)
  | Deposit () -> deposit state ()
  | Withdraw amnt -> withdraw state amnt
  | Set_delegate kho -> set_delegate state kho
  (* Internal entrypoints *)
  | Call_vault_receive_tez p -> call_vault_receive_tez state p
  | Call_vault_send_tez_to_contract p -> call_vault_send_tez_to_contract state p
  | Call_vault_send_tez_to_vault p -> call_vault_send_tez_to_vault state p
  | Call_vault_set_delegate p -> call_vault_set_delegate state p

(*****************************************************************************)
(**                       {1 OFFLINE FA2 VIEWS}                              *)
(*****************************************************************************)

let view_get_balance ((owner, token_id), state: (address * fa2_token_id) * wtez_state) : nat =
  fa2_get_balance (state.fa2_state, owner, token_id)

let view_total_supply (token_id, state: fa2_token_id * wtez_state) : nat =
  if token_id = wtez_token_id then
    state.total_token
  else
    failwith "FA2_TOKEN_UNDEFINED"

let view_all_tokens ((), _state: unit * wtez_state) : fa2_token_id list =
  [ wtez_token_id ]

let view_is_operator ((owner, (operator, token_id)), state: (address * (address * fa2_token_id)) * wtez_state) : bool =
  fa2_is_operator (state.fa2_state, operator, owner, token_id)
