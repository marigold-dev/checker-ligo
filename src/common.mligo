(* open Error *)

(* OPERATIONS ON int *)
[@inline] let int_zero = (0)

let min_int (x: int) (y: int) = if leq_int_int x y then x else y
let max_int (x: int) (y: int) = if geq_int_int x y then x else y

let neg_int (x: int) = sub_int_int ((0)) x

(* Note that ligo is not happy with nested lets. Take out when ready, but
 * keep internal for now. *)
let rec pow_rec (y, x, n: int * int * nat) : int =
  if eq_nat_nat n (0n) then
    y
  else if eq_nat_nat n (1n) then
    mul_int_int x y
  else
    match ediv_nat_nat n (2n) with
    (* Note: Ignoring coverage for this line because it is unreachable *)
    | None -> (failwith internalError_PowRecImpossible : int)
              (* [@coverage off] *)
    | Some quot_rem ->
      let (quot, rem) = quot_rem in
      if eq_nat_nat rem (0n) then
        pow_rec (y, mul_int_int x x, quot)
      else
        pow_rec (mul_int_int x y, mul_int_int x x, quot)

let pow_int_nat (x: int) (n: nat) = pow_rec ((1), x, n)

let cdiv_int_int (x: int) (y: int) =
  match ediv_int_int x y with
  | None -> (failwith internalError_CdivIntIntZeroDenominator : int)
  | Some quot_rem ->
    let (quot, rem) = quot_rem in
    if eq_nat_nat rem (0n) then
      quot
    else if lt_int_int y ((0)) then
      quot
    else
      add_int_int quot ((1))

let fdiv_int_int (x: int) (y: int) =
  match ediv_int_int x y with
  | None -> (failwith internalError_FdivIntIntZeroDenominator : int)
  | Some quot_rem ->
    let (quot, rem) = quot_rem in
    if eq_nat_nat rem (0n) then
      quot
    else if gt_int_int y ((0)) then
      quot
    else
      sub_int_int quot ((1))

let clamp_int (v: int) (lower: int) (upper: int) : int =

  min_int upper (max_int v lower)

(* OPERATIONS ON tez *)
[@inline] let tez_to_mutez_nat (amnt: tez) = div_tez_tez amnt (1mutez)
[@inline] let tez_of_mutez_nat (amnt: nat) = mul_nat_tez amnt (1mutez)

let tez_to_mutez (x: tez) = int (tez_to_mutez_nat x)

let tez_zero : tez = 0mutez

(* OPERATIONS ON nat *)
let min_nat (x: nat) (y: nat) = if leq_nat_nat x y then x else y
let max_nat (x: nat) (y: nat) = if geq_nat_nat x y then x else y

(* RATIOS AND OPERATIONS ON THEM *)

(** A rational is represented as a pair numerator/denominator, reduced to have
  * a positive denominator. This form is canonical. *)
type ratio = {
  num: int; (** Numerator. *)
  den: int; (** Denominator, > 0 *)
}

(* The denominator must be positive. *)
[@inline] let make_ratio (n: int) (d: int) : ratio =

  { num = n; den = d; }

(* zero: 0/1 *)
[@inline] let zero_ratio : ratio = { num = (0); den = (1); }

(* one: 1/1 *)
[@inline] let one_ratio : ratio = { num = (1); den = (1); }

(* Floor a fraction to a natural number. *)
let fraction_to_nat_floor (x_num: int) (x_den: int) : nat =

  match is_nat x_num with
  | None -> (failwith internalError_FractionToNatFloorNegative : nat)
  | Some n ->
    (match ediv_nat_nat n (abs x_den) with
     (* Note: Ignoring coverage for the case below since the assertion above makes it unreachable in OCaml *)
     | None -> (failwith internalError_FractionToNatFloorZeroDenominator : nat)
               (* [@coverage off] *)
     | Some quot_and_rem ->
       let (quot, _) = quot_and_rem in
       quot (* ignore the remainder; we floor towards zero here *)
    )

(* Ensure that there is no tez given. To prevent accidental fund loss. *)
let ensure_no_tez_given () =
  if Tezos.amount <> 0mutez
  then failwith error_UnwantedTezGiven
  else ()

(* BEGIN_OCAML   
(* [@@@coverage off] *)
let compare_int (i: int) (j: int) : Int.t =
  if gt_int_int i j then
    1
  else if eq_int_int i j then
    0
  else
    -1

let compare_nat (i: nat) (j: nat) : Int.t =
  if gt_nat_nat i j then
    1
  else if eq_nat_nat i j then
    0
  else
    -1

let show_ratio n = (string_of_int n.num) ^ "/" ^ (string_of_int n.den)
let pp_ratio f x = Format.pp_print_string f (show_ratio x)
(* [@@@coverage on] *)
   END_OCAML *)
