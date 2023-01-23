(* [@@@coverage off] *)

type burrow_storage =
  { checker_address: address;
    collateral_fa2: address;
  }
(* [@@deriving show] *)

type burrow_parameter =
  | BurrowSetDelegate of key_hash option
  | BurrowTransfer of (address * nat)
(* [@@deriving show] *)

(* [@@@coverage on] *)
