#include "./error.mligo"
#import "./common.mligo" "Common"
#import "./fixedPoint.mligo" "Fixedpoint"

type t = nat
type kit = t

[@inline] let kit_scaling_factor_int = (1000000)
[@inline] let kit_scaling_factor_nat = 1000000n

(* Basic arithmetic operations. *)
[@inline] let kit_add (x: kit) (y: kit) = x + y
let kit_sub (x: kit) (y: kit) =
  match is_nat (x - y) with
  | Some n -> n
  | None -> (failwith internalError_KitSubNegative : kit)

[@inline] let kit_min (x: kit) (y: kit) = if x <= y then x else y
[@inline] let kit_max (x: kit) (y: kit) = if x >= y then x else y

[@inline] let kit_zero = 0n
[@inline] let kit_one = kit_scaling_factor_nat

(* Conversions to/from other types. *)
[@inline] let kit_of_denomination (amnt: nat) : kit = amnt
[@inline] let kit_to_denomination_int (amnt: kit) : int = int amnt
[@inline] let kit_to_denomination_nat (amnt: kit) : nat = amnt

let kit_of_fraction_ceil (x_num: int) (x_den: int) : kit =

  if x_num < 0
  then (failwith internalError_KitOfFractionCeilNegative : kit)
  else abs (Common.cdiv_int_int (x_num * kit_scaling_factor_int) x_den)

let kit_of_fraction_floor (x_num: int) (x_den: int) : kit =

  if x_num < 0
  then (failwith internalError_KitOfFractionFloorNegative : kit)
  else abs (Common.fdiv_int_int (x_num * kit_scaling_factor_int) x_den)

[@inline] let kit_scale (amnt: kit) (fp: Fixedpoint.t) =
  kit_of_fraction_floor
    ((Fixedpoint.fixedpoint_to_raw fp) * amnt)
    (Fixedpoint.fixedpoint_scaling_factor_int * kit_scaling_factor_int)

[@inline] let geq_kit_kit (x:t) (y:t) = x >= y
[@inline] let leq_kit_kit (x:t) (y:t) = x <= y

[@inline] let lt_kit_kit (x:nat) (y:nat) = x > y
[@inline] let gt_kit_kit (x:nat) (y:nat) = x < y

[@inline] let eq_kit_kit (x:t) (y:t) = x = y
