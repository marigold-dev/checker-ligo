(* open BurrowTypes *)
(* open CheckerTypes *)
(* open Fa2Interface *)

[@inline] let originate_burrow (state: checker) (delegate_opt: key_hash option) : operation * address =
  Tezos.create_contract
    (fun (p, storage : burrow_parameter * burrow_storage) ->
       if Tezos.get_sender () <> storage.checker_address then
         (failwith ((-1)) : operation list * burrow_storage)
       else if Tezos.get_amount () <> 0mutez then
         (failwith ((-2)) : operation list * burrow_storage)
       else
         match p with
         | BurrowSetDelegate kho ->
           (* NOTE: this deviates slightly from the design in the issue. *)
           let op = match (Tezos.get_entrypoint_opt "%set_delegate" storage.collateral_fa2 : key_hash option contract option) with
             | Some c -> Tezos.transaction kho (0mutez) c
             | None -> (failwith ((-3)) : operation) in (* i.e., set_delegate not supported *)
           ([op], storage)
         | BurrowTransfer p ->
           let (addr, amnt) = p in
           let transfer =
             { from_ = Tezos.get_self_address (); (* from: FA2 account of burrow contract *)
               txs = [
                 { to_ = addr;                   (* to: FA2 account of the given address *)
                   token_id = 2n;
                   amount = amnt;
                 }
               ];
             } in
           let fa2_transfer_contract =
             match (Tezos.get_entrypoint_opt "%transfer" storage.collateral_fa2 : (fa2_transfer list) contract option) with
             | Some c -> c
             | None -> (failwith ((-4)) : (fa2_transfer list) contract) in
           let op =
             Tezos.transaction
               [transfer] (0mutez)
               fa2_transfer_contract in
           ([op], storage)
    )
    delegate_opt
    (0mutez) (* initially no collateral in it *)
    { checker_address = Tezos.get_self_address ();
      collateral_fa2 = state.external_contracts.collateral_fa2;
    }
