#include "./error.mligo"
#import "./common.mligo" "Common"
#import "./fixedPoint.mligo" "Fixedpoint"

(* open Common *)
(* open Error *)
(* open FixedPoint *)
(* open TokenMetadata *)

type t = nat
type tok = t

[@inline] let tok_scaling_factor_int = (1000000)
[@inline] let tok_scaling_factor_nat = 1000000n

(* Basic arithmetic operations. *)
[@inline] let tok_add (x: tok) (y: tok) = x + y
let tok_sub (x: tok) (y: tok) =
  match is_nat (x - y) with
  | Some n -> n
  | None -> (failwith internalError_TokSubNegative : tok)

[@inline] let tok_min (x: tok) (y: tok) = if x <= y then x else y
[@inline] let tok_max (x: tok) (y: tok) = if x >= y then x else y

[@inline] let tok_zero = 0n
[@inline] let tok_one = tok_scaling_factor_nat

(* Conversions to/from other types. *)
[@inline] let tok_of_denomination (amnt: nat) : tok = amnt
[@inline] let tok_to_denomination_int (amnt: tok) : int = int amnt
[@inline] let tok_to_denomination_nat (amnt: tok) : nat = amnt

let tok_of_fraction_ceil (x_num: int) (x_den: int) : tok =

  if x_num < 0
  then (failwith internalError_TokOfFractionCeilNegative : tok)
  else abs (Common.cdiv_int_int (x_num * tok_scaling_factor_int) x_den)

let tok_of_fraction_floor (x_num: int) (x_den: int) : tok =

  if x_num < 0
  then (failwith internalError_TokOfFractionFloorNegative : tok)
  else abs (Common.fdiv_int_int (x_num * tok_scaling_factor_int) x_den)

[@inline] let tok_scale (amnt: tok) (fp: Fixedpoint.t) =
  tok_of_fraction_floor
    ((Fixedpoint.fixedpoint_to_raw fp) * amnt)
    (Fixedpoint.fixedpoint_scaling_factor_int * tok_scaling_factor_int)

[@inline] let geq_tok_tok (x:t) (y:t) = x >= y
[@inline] let leq_tok_tok (x:t) (y:t) = x <= y

[@inline] let lt_tok_tok (x:t) (y:t) = x < y
[@inline] let gt_tok_tok (x:t) (y:t) = x > y

[@inline] let eq_tok_tok (x:t) (y:t) = x = y

(* BEGIN_OCAML
(* [@@@coverage off] *)
[@inline] let tok_to_ratio (amnt: tok) : ratio = make_ratio (int amnt) tok_scaling_factor_int

let tok_compare x y = compare_nat x y

let show_tok amnt =
  let zfill s width = match Stdlib.(width - (String.length s)) with
    | to_fill when to_fill <= 0 -> s
    | to_fill -> (String.make to_fill '0') ^ s
  in
  let as_string =
    if tok_decimal_digits = 0n then
      string_of_nat amnt
    else
      let d, r = Option.get (ediv_nat_nat amnt tok_scaling_factor_nat) in
      let tok_decimal_digits = Stdlib.int_of_string (string_of_nat tok_decimal_digits) in (* little hacky *)
      (string_of_nat d) ^ "." ^ zfill (string_of_nat r) tok_decimal_digits
  in as_string ^ "tok"

let pp_tok ppf amnt = Format.fprintf ppf "%s" (show_tok amnt)

(* [@@@coverage on] *)
   END_OCAML *)
