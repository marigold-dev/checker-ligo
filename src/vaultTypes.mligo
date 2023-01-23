(* [@@@coverage off] *)

type vault_storage = { owner : address; }
(* [@@deriving show] *)

type vault_parameter =
  | Vault_set_delegate of key_hash option
  | Vault_receive_tez of unit
  | Vault_send_tez_to_vault of (tez * address)
  | Vault_send_tez_to_contract of (tez * address)
(* [@@deriving show] *)

(* [@@@coverage on] *)
