type state =
  { price: (nat * nat);
    owner: address;
  }

let make_storage (owner: address) =
  {
    price = (1n, 1n);
    owner = owner
  }

[@entry]
let main (new_price: nat * nat) (state: state): operation list * state =
  if Tezos.get_sender () = state.owner
  then [], {state with price = new_price }
  else failwith "unauthorized"

[@view]
let get_price ((), state: unit * state): (nat * nat) =
  state.price
