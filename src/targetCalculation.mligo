#import "./common.mligo" "Common"
#import "./fixedPoint.mligo" "Fixedpoint"

(** Calculate the current target based on the current quantity, the current
    index, and the current price of kit in tok.
    {[
      target_{i+1} = FLOOR (q_{i+1} * index_{i+1} / kit_in_tok_{i+1})
    ]}
*)
[@inline] let compute_current_target (current_q: Fixedpoint.t) (current_index: Fixedpoint.t)
  (current_kit_in_tok: Common.ratio) : Fixedpoint.t =
  let { num = num; den = den; } = current_kit_in_tok in
  Fixedpoint.fixedpoint_of_ratio_floor
    (Common.make_ratio
       (
          den
          *
          (
             (Fixedpoint.fixedpoint_to_raw current_q)
             *
             (Fixedpoint.fixedpoint_to_raw current_index)
          )
       )
       (
          num
          *
          (
             Fixedpoint.fixedpoint_scaling_factor_int
             *
             Fixedpoint.fixedpoint_scaling_factor_int
          )
       )
    )
