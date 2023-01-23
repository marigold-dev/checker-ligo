(** Type of data used when invoking the %transfer entrypoint. *)

(* [@@@coverage off] *)

type fa12_transfer =
  (* BEGIN_LIGO *) [@layout:comb] (* END_LIGO *)
  { (* BEGIN_LIGO *) [@annot:from] (* END_LIGO *) address_from : address;
                                          (* BEGIN_LIGO *) [@annot:to] (* END_LIGO *) address_to : address;
                                          value : nat }
(* [@@deriving show] *)

(* [@@@coverage on] *)
