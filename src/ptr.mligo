(* [@@@coverage off] *)
type ptr = nat
(* [@@deriving show] *)
(* [@@@coverage on] *)

[@inline] let ptr_null = 0n
[@inline] let ptr_next (t: ptr) = add_nat_nat t (1n)

(* BEGIN_OCAML   
(* [@@@coverage off] *)
let compare_ptr = Common.compare_nat
let random_ptr () = nat_from_int64 (Random.int64 Int64.max_int)
(* [@@@coverage on] *)
   END_OCAML *)
