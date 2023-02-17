(* open Common *)
#include "./common.mligo"

type t = int
type fixedpoint = t

let fixedpoint_scaling_factor_int = 18446744073709551616  (* 2 (scaling_base) ^ 64 (scaling_exponent) *)
let fixedpoint_scaling_factor_nat = 18446744073709551616n (* 2 (scaling_base) ^ 64 (scaling_exponent) *)

(* Predefined values. *)
[@inline] let fixedpoint_zero = (0)
[@inline] let fixedpoint_one = fixedpoint_scaling_factor_int

(* Arithmetic operations. *)
[@inline] let fixedpoint_add (x: fixedpoint) (y: fixedpoint) = x + y
[@inline] let fixedpoint_sub (x: fixedpoint) (y: fixedpoint) = x - y

let fixedpoint_pow (x: fixedpoint) (y: nat) =
  if y = (0n) then
    fixedpoint_one
  else
      (pow_int_nat x y) / (pow_int_nat fixedpoint_scaling_factor_int (abs (y - 1n)))

[@inline] let fixedpoint_min (x: fixedpoint) (y: fixedpoint) = min_int x y
[@inline] let fixedpoint_max (x: fixedpoint) (y: fixedpoint) = max_int x y

(* Conversions to/from other types. *)
let fixedpoint_of_ratio_ceil  (amnt: ratio) = cdiv_int_int (amnt.num * fixedpoint_scaling_factor_int) amnt.den
let fixedpoint_of_ratio_floor (amnt: ratio) = fdiv_int_int (amnt.num * fixedpoint_scaling_factor_int) amnt.den
(* George: do we need flooring-division or truncating-division? more thought is needed *)

[@inline] let fixedpoint_of_raw (amnt: int) : fixedpoint = amnt
[@inline] let fixedpoint_to_raw (amnt: fixedpoint) : int = amnt

(* BEGIN_OCAML
(* [@@@coverage off] *)
let fixedpoint_scaling_exponent = 64

let fixedpoint_to_ratio (amnt: fixedpoint) = make_ratio amnt fixedpoint_scaling_factor_int

let fixedpoint_of_hex_string str =
  let without_dot = Str.replace_first (Str.regexp (Str.quote ".")) "" str in
  let dotpos = String.rindex_opt str '.' in
  let mantissa = match dotpos with
    | None -> (1)
    | Some pos -> pow_int_nat ((16)) (abs (int_from_literal (string_of_int (String.length str - pos - 1)))) in
  div_int_int (mul_int_int (of_string_base_int 16 without_dot) fixedpoint_scaling_factor_int) mantissa

let show_fixedpoint amnt =
  let zfill s width =
    let to_fill = (width - (String.length s)) in
    if to_fill <= 0
    then s
    else (String.make to_fill '0') ^ s in

  let sign = if amnt < (0) then "-" else "" in
  let (upper, lower) = div_rem_int_int (int (abs amnt)) fixedpoint_scaling_factor_int in

  (* in hex, otherwise it's massive *)
  Format.sprintf "%s%s.%s"
    sign
    (format_int "%X" upper)
    (zfill (format_int "%X" lower) (fixedpoint_scaling_exponent / 4))

let pp_fixedpoint ppf amnt = Format.fprintf ppf "%s" (show_fixedpoint amnt)

let show_fixedpoint_raw amnt = string_of_int (fixedpoint_to_raw amnt)
(* [@@@coverage on] *)
   END_OCAML *)
