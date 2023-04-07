type state =
  { price: (nat * nat);
    owner: address;
  }

let make_storage (owner: address) =
  {
    price = (1n, 1n);
    owner = owner
  }

type params =
  | Update of (nat * nat)
  | GetCfmmPrice of (nat * nat) contract

let main (op: params) (state: state): operation list * state =
  match op with
  | GetCfmmPrice cb ->
    let op = Tezos.transaction state.price 0mutez cb in
    ([op], state)
  | Update new_price ->
    if Tezos.get_sender () = state.owner
    then (([]: operation list), {state with price = new_price })
    else failwith "unauthorized"
