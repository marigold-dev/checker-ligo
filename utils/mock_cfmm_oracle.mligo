type state =
  {
    owner: address;
    price: (nat * nat);
  }

let make_storage (owner: address) =
  {
    owner = owner;
    price = (1n, 1n);
  }

type params =
  | Update of (nat * nat)
  | GetCfmmPrice of (nat * nat) contract

[@entry]
let main (op: params) (state: state): operation list * state =
  match op with
  | GetCfmmPrice cb ->
    let op = Tezos.transaction state.price 0mutez cb in
    ([op], state)
  | Update new_price ->
    if Tezos.get_sender () = state.owner
    then (([]: operation list), {state with price = new_price })
    else failwith "unauthorized"
