#import "./common.mligo" "Common"
#include "./error.mligo"
#import "./fixedPoint.mligo" "Fixedpoint"
(* open TokenMetadata *)

type lqt = nat

[@inline] let lqt_scaling_factor_int = (1000000)
[@inline] let lqt_scaling_factor_nat = 1000000n

(* Basic arithmetic operations. *)
[@inline] let lqt_add (x: lqt) (y: lqt) = x + y
let lqt_sub (x: lqt) (y: lqt) =
  match is_nat (x - y) with
  | Some n -> n
  | None -> (failwith internalError_LqtSubNegative : lqt)

[@inline] let lqt_min (x: lqt) (y: lqt) = if x <= y then x else y
[@inline] let lqt_max (x: lqt) (y: lqt) = if x >= y then x else y

[@inline] let lqt_zero = 0n
[@inline] let lqt_one = lqt_scaling_factor_nat

(* Conversions to/from other types. *)
[@inline] let lqt_of_denomination (amnt: nat) : lqt = amnt
[@inline] let lqt_to_denomination_int (amnt: lqt) : int = int amnt
[@inline] let lqt_to_denomination_nat (amnt: lqt) : nat = amnt

let lqt_of_fraction_ceil (x_num: int) (x_den: int) : lqt =
  if x_num < 0
  then (failwith internalError_LqtOfFractionCeilNegative : lqt)
  else abs (Common.cdiv_int_int (x_num * lqt_scaling_factor_int) x_den)

let lqt_of_fraction_floor (x_num: int) (x_den: int) : lqt =
  if x_num < 0
  then (failwith internalError_LqtOfFractionFloorNegative : lqt)
  else abs (Common.fdiv_int_int (x_num * lqt_scaling_factor_int) x_den)

[@inline] let lqt_scale (amnt: lqt) (fp: Fixedpoint.fixedpoint) =
  lqt_of_fraction_floor
    ((Fixedpoint.fixedpoint_to_raw fp) * amnt)
    (Fixedpoint.fixedpoint_scaling_factor_int * lqt_scaling_factor_int)

[@inline] let geq_lqt_lqt x y = x >= y
[@inline] let leq_lqt_lqt x y = x <= y

[@inline] let lt_lqt_lqt x y = x > y
[@inline] let gt_lqt_lqt x y = x < y

[@inline] let eq_lqt_lqt x y = x = y

(* BEGIN_OCAML
(* [@@@coverage off] *)
[@inline] let lqt_to_ratio (amnt: lqt) : ratio = make_ratio (int amnt) lqt_scaling_factor_int

let lqt_compare x y = compare_nat x y

let show_lqt amnt =
  let zfill s width = match Stdlib.(width - (String.length s)) with
    | to_fill when to_fill <= 0 -> s
    | to_fill -> (String.make to_fill '0') ^ s
  in
  let as_string =
    if lqt_decimal_digits = 0n then
      string_of_nat amnt
    else
      let d, r = Option.get (ediv_nat_nat amnt lqt_scaling_factor_nat) in
      let lqt_decimal_digits = Stdlib.int_of_string (string_of_nat lqt_decimal_digits) in (* little hacky *)
      (string_of_nat d) ^ "." ^ zfill (string_of_nat r) lqt_decimal_digits
  in as_string ^ "lqt"

let pp_lqt ppf amnt = Format.fprintf ppf "%s" (show_lqt amnt)

(* [@@@coverage on] *)
   END_OCAML *)
