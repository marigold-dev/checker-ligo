#import "./common.mligo" "Common"
#include "./error.mligo"
#import "./fixedPoint.mligo" "Fixedpoint"
(* open TokenMetadata *)

type ctok = nat

[@inline] let ctok_scaling_factor_int = (1000000)
[@inline] let ctok_scaling_factor_nat = 1000000n

(* Basic arithmetic operations. *)
[@inline] let ctok_add (x: ctok) (y: ctok) = x + y
let ctok_sub (x: ctok) (y: ctok) =
  match is_nat (x - y) with
  | Some n -> n
  | None -> (failwith internalError_CtokSubNegative : ctok)

[@inline] let ctok_min (x: ctok) (y: ctok) = if x <= y then x else y
[@inline] let ctok_max (x: ctok) (y: ctok) = if x >= y then x else y

[@inline] let ctok_zero = 0n
[@inline] let ctok_one = ctok_scaling_factor_nat

(* Conversions to/from other types. *)
[@inline] let ctok_of_denomination (amnt: nat) : ctok = amnt
[@inline] let ctok_to_denomination_int (amnt: ctok) : int = int amnt
[@inline] let ctok_to_denomination_nat (amnt: ctok) : nat = amnt

let ctok_of_fraction_ceil (x_num: int) (x_den: int) : ctok =
  if x_num < 0
  then (failwith internalError_CtokOfFractionCeilNegative : ctok)
  else abs (Common.cdiv_int_int (x_num * ctok_scaling_factor_int) x_den)

let ctok_of_fraction_floor (x_num: int) (x_den: int) : ctok =
  if x_num < 0
  then (failwith internalError_CtokOfFractionFloorNegative : ctok)
  else abs (Common.fdiv_int_int (x_num * ctok_scaling_factor_int) x_den)

[@inline] let ctok_scale (amnt: ctok) (fp: Fixedpoint.fixedpoint) =
  ctok_of_fraction_floor
    ((Fixedpoint.fixedpoint_to_raw fp) * amnt)
    (Fixedpoint.fixedpoint_scaling_factor_int * ctok_scaling_factor_int)

[@inline] let geq_ctok_ctok x y = x >= y
[@inline] let leq_ctok_ctok x y = x <= y

[@inline] let lt_ctok_ctok x y = x > y
[@inline] let gt_ctok_ctok x y = x < y

[@inline] let eq_ctok_ctok (x: ctok) (y: ctok) = x = y

(* BEGIN_OCAML
(* [@@@coverage off] *)
[@inline] let ctok_to_ratio (amnt: ctok) : ratio = make_ratio (int amnt) ctok_scaling_factor_int

let ctok_compare x y = compare_nat x y

let show_ctok amnt =
  let zfill s width = match Stdlib.(width - (String.length s)) with
    | to_fill when to_fill <= 0 -> s
    | to_fill -> (String.make to_fill '0') ^ s
  in
  let as_string =
    if ctok_decimal_digits = 0n then
      string_of_nat amnt
    else
      let d, r = Option.get (ediv_nat_nat amnt ctok_scaling_factor_nat) in
      let ctok_decimal_digits = Stdlib.int_of_string (string_of_nat ctok_decimal_digits) in (* little hacky *)
      (string_of_nat d) ^ "." ^ zfill (string_of_nat r) ctok_decimal_digits
  in as_string ^ "ctok"

let pp_ctok ppf amnt = Format.fprintf ppf "%s" (show_ctok amnt)

(* [@@@coverage on] *)
   END_OCAML *)
