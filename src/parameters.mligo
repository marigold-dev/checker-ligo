(* open Kit *)
(* open FixedPoint *)
(* open Common *)
(* open Constants *)
(* open DriftDerivative *)
(* open TargetCalculation *)

(* [@@@coverage off] *)

type parameters =
  { q : fixedpoint; (* 1/kit, really *)
    index: fixedpoint;
    protected_index: fixedpoint;
    target: fixedpoint;
    drift_derivative: fixedpoint;
    drift: fixedpoint;
    burrow_fee_index: fixedpoint;
    imbalance_index: fixedpoint;
    outstanding_kit: kit;
    circulating_kit: kit;
    last_touched: timestamp;
  }
(* [@@deriving show] *)

(* [@@@coverage on] *)

(** Initial state of the parameters. *)
let initial_parameters : parameters =
  { q = fixedpoint_one;
    index = fixedpoint_one;
    protected_index = fixedpoint_one;
    target = fixedpoint_one;
    drift = fixedpoint_zero;
    drift_derivative = fixedpoint_zero;
    burrow_fee_index = fixedpoint_one;
    imbalance_index = fixedpoint_one;
    outstanding_kit = kit_zero;
    circulating_kit = kit_zero;
    last_touched = Tezos.get_now ();
  }

(** Compute the current minting index (in tok). To get tok/kit must multiply with q. *)
[@inline] let tz_minting (p: parameters) : fixedpoint = fixedpoint_max p.index p.protected_index

(** Compute the current liquidation index (in tok). To get tok/kit must multiply with q. *)
[@inline] let tz_liquidation (p: parameters) : fixedpoint = fixedpoint_min p.index p.protected_index

(** Current minting price (in tok/kit). *)
let minting_price (p: parameters) : ratio =
  make_ratio
    (mul_int_int (fixedpoint_to_raw p.q) (fixedpoint_to_raw (tz_minting p)))
    (mul_int_int fixedpoint_scaling_factor_int fixedpoint_scaling_factor_int)

(** Current liquidation price (in tok/kit). *)
let liquidation_price (p: parameters) : ratio =
  make_ratio
    (mul_int_int (fixedpoint_to_raw p.q) (fixedpoint_to_raw (tz_liquidation p)))
    (mul_int_int fixedpoint_scaling_factor_int fixedpoint_scaling_factor_int)

(** Given the amount of kit necessary to close all existing burrows
    (outstanding) and the amount of kit that is currently in circulation
    (circulating), compute the current imbalance adjustment using the following
    formula:
    {[
      clamp
        ( imbalance_scaling_factor * (circulating - outstanding) / circulating,
          -imbalance_limit,
          +imbalance_limit
        )
    ]}

    or, equivalently,
    {[
      min (imbalance_scaling_factor * (circulating - outstanding) / circulating, +imbalance_limit), if circulating >= outstanding
                                                                                                         max (imbalance_scaling_factor * (circulating - outstanding) / circulating, -imbalance_limit), if circulating < outstanding
    ]}

    Edge cases:
    - [circulating = 0] and [outstanding = 0].
        The imbalance fee/bonus is 0.
    - [circulating = 0] and [outstanding > 0].
        Well, outstanding is "infinitely" greater than circulating so let's
        saturate the imbalance to -imbalance_limit.
*)
[@inline] let compute_imbalance (outstanding: kit) (circulating: kit) : ratio =
  let outstanding = kit_to_denomination_nat outstanding in
  let circulating = kit_to_denomination_nat circulating in
  let { num = num_il; den = den_il; } = imbalance_limit in

  if (eq_nat_nat circulating (0n))
  && (eq_nat_nat outstanding (0n)) then
    zero_ratio
  else if (eq_nat_nat circulating (0n))
       && (ne_nat_nat outstanding (0n)) then
    make_ratio (neg_int num_il) den_il
  else
    let { num = num_isf; den = den_isf; } = imbalance_scaling_factor in
    let denominator = mul_int_nat den_isf circulating in

    if geq_nat_nat circulating outstanding then
      make_ratio
        (min_int (mul_int_int (mul_int_int num_isf (sub_nat_nat circulating outstanding)) den_il) (mul_int_int num_il denominator))
        (mul_int_int den_il denominator)
    else (* circulating < outstanding *)
      begin

        make_ratio
          (neg_int (min_int (mul_int_int (mul_int_int num_isf (sub_nat_nat outstanding circulating)) den_il) (mul_int_int num_il denominator)))
          (mul_int_int den_il denominator)
      end

(** Compute the current adjustment index. Basically this is the product of
    the burrow fee index and the imbalance adjustment index.
    {[
      adjustment_index_i = FLOOR (burrow_fee_index_i * imabalance_index_i)
    ]}
*)
let compute_adjustment_index (p: parameters) : fixedpoint =
  fixedpoint_of_raw
    (fdiv_int_int
       (mul_int_int
          (fixedpoint_to_raw p.burrow_fee_index)
          (fixedpoint_to_raw p.imbalance_index)
       )
       fixedpoint_scaling_factor_int
    )

(** Calculate the current burrow fee index based on the last index and the
    number of seconds that have elapsed.
    {[
      burrow_fee_index_{i+1} = FLOOR (burrow_fee_index_i * (1 + burrow_fee_percentage * ((t_{i+1} - t_i) / <seconds_in_a_year>)))
    ]}
    Keep in mind that this formula means that the burrow fee index is
    ever-increasing. *)
[@inline] let compute_current_burrow_fee_index (last_burrow_fee_index: fixedpoint) (duration_in_seconds: int) : fixedpoint =
  let { num = num; den = den; } = burrow_fee_percentage in
  let denom = mul_int_int den seconds_in_a_year in
  fixedpoint_of_raw
    (fdiv_int_int
       (mul_int_int
          (fixedpoint_to_raw last_burrow_fee_index)
          (add_int_int
             denom
             (mul_int_int num duration_in_seconds)
          )
       )
       denom
    )

(** Calculate the current protected index based on the last protected index,
    the current index (as provided by the oracle), and the number of seconds
    that have elapsed.
    {[
      protected_index_{i+1} = FLOOR (
        protected_index_i * CLAMP (index_{i+1}/protected_index_i, EXP(-epsilon * (t_{i+1} - t_i)), EXP(+epsilon * (t_{i+1} - t_i)))
      )
    ]}
*)
[@inline] let compute_current_protected_index (last_protected_index: fixedpoint) (current_index: fixedpoint) (duration_in_seconds: int) : fixedpoint =


  fixedpoint_of_ratio_floor
    (make_ratio
       (clamp_int
          (mul_int_int
             (fixedpoint_to_raw current_index)
             protected_index_inverse_epsilon
          )
          (mul_int_int
             (fixedpoint_to_raw last_protected_index)
             (sub_int_int
                protected_index_inverse_epsilon
                duration_in_seconds
             )
          )
          (mul_int_int
             (fixedpoint_to_raw last_protected_index)
             (add_int_int
                protected_index_inverse_epsilon
                duration_in_seconds
             )
          )
       )
       (mul_int_int
          protected_index_inverse_epsilon
          fixedpoint_scaling_factor_int
       )
    )

(** Calculate the current drift based on the last drift, the last drift
    derivative, the current drift derivative, and the number of seconds that
    have elapsed.
    {[
      drift_{i+1} = FLOOR (drift_i + (1/2) * (drift'_i + drift'_{i+1}) * (t_{i+1} - t_i))
    ]}
*)
[@inline] let compute_current_drift (last_drift: fixedpoint) (last_drift_derivative: fixedpoint) (current_drift_derivative: fixedpoint) (duration_in_seconds: int) : fixedpoint =
  fixedpoint_of_raw
    (fdiv_int_int
       (add_int_int
          (mul_int_int ((2)) (fixedpoint_to_raw last_drift))
          (mul_int_int
             (fixedpoint_to_raw (fixedpoint_add last_drift_derivative current_drift_derivative))
             duration_in_seconds
          )
       )
       ((2))
    )

(** Calculate the current quantity based on the last quantity, the last drift,
    the last drift derivative, the current drift derivative, and the number of
    seconds that have elapsed.
    {[
      q_{i+1} = FLOOR (q_i * EXP((drift_i + (1/6) * (2*drift'_i + drift'_{i+1}) * (t_{i+1} - t_i)) * (t_{i+1} - t_i)))
    ]}
    where [EXP(X) = X+1].
*)
[@inline] let compute_current_q (last_q: fixedpoint) (last_drift: fixedpoint) (last_drift_derivative: fixedpoint) (current_drift_derivative: fixedpoint) (duration_in_seconds: int) : fixedpoint =
  let six_sf =
    mul_int_int
      ((6))
      fixedpoint_scaling_factor_int in
  fixedpoint_of_raw
    (fdiv_int_int
       (mul_int_int
          (fixedpoint_to_raw last_q)
          (add_int_int
             (mul_int_int
                (add_int_int
                   (mul_int_int
                      ((6))
                      (fixedpoint_to_raw last_drift)
                   )
                   (mul_int_int
                      (add_int_int
                         (mul_int_int
                            ((2))
                            (fixedpoint_to_raw last_drift_derivative)
                         )
                         (fixedpoint_to_raw current_drift_derivative)
                      )
                      duration_in_seconds
                   )
                )
                duration_in_seconds
             )
             six_sf
          )
       )
       six_sf
    )

(** Calculate the current imbalance index based on the last amount of
    outstanding kit, the last amount of circulating kit, the last imbalance
    index, and the number of seconds that have elapsed, using the following
    formula:
    {[
      imbalance_index_{i+1} = FLOOR (
        imbalance_index_i * (1 + imbalance * (t_{i+1} - t_i) / <seconds_in_a_year>)
      )
    ]}
    (note that the last outstanding kit and circulating kit are used in the
    calculation of imbalance; see {!compute_imbalance}).

    This calculation means that even if the imbalance_rate is bounded (from -5
    cNp to +5 cNp), the imbalance index is not. The above formula for
    [imbalance = -5cNp] and 20 years time in seconds elapsed gives
    [imbalance_index_{i+1} = 0] (for longer time it gives
    [imbalance_index_{i+1} < 0]). This can make calculations below fail. All of
    this of course refers to the possibility of nobody touching checker for
    over 20 years, which I guess should be practically impossible. *)
[@inline] let compute_current_imbalance_index (last_outstanding_kit: kit) (last_circulating_kit: kit) (last_imbalance_index: fixedpoint) (duration_in_seconds: int) : fixedpoint =
  let { num = num; den = den; } =
    compute_imbalance
      last_outstanding_kit (* burrowed *)
      last_circulating_kit (* circulating *) in
  let denom = mul_int_int den seconds_in_a_year in
  fixedpoint_of_raw
    (fdiv_int_int
       (mul_int_int
          (fixedpoint_to_raw last_imbalance_index)
          (add_int_int
             denom
             (mul_int_int num duration_in_seconds)
          )
       )
       denom
    )

(** Compute current outstanding kit, taking burrow fees into account:
    {[
      outstanding_with_fees_{i+1} = FLOOR (
        outstanding_kit_i * burrow_fee_index_{i+1} / burrow_fee_index_i
      )
    ]}
*)
[@inline] let compute_current_outstanding_with_fees (last_outstanding_kit: kit) (last_burrow_fee_index: fixedpoint) (current_burrow_fee_index: fixedpoint) : kit =
  kit_of_fraction_floor
    (mul_nat_int (kit_to_denomination_nat last_outstanding_kit) (fixedpoint_to_raw current_burrow_fee_index))
    (mul_int_int kit_scaling_factor_int (fixedpoint_to_raw last_burrow_fee_index))

(** Compute current outstanding kit, given that the burrow fees have already
    been added (that is, compute the effect of the imbalance index):
    {[
      outstanding_kit_{i+1} = FLOOR (
        outstanding_with_fees_{i+1} * imbalance_index_{i+1} / imbalance_index_i
      )
    ]}
*)
[@inline] let compute_current_outstanding_kit (current_outstanding_with_fees: kit) (last_imbalance_index: fixedpoint) (current_imbalance_index: fixedpoint) : kit =
  kit_of_fraction_floor
    (mul_nat_int (kit_to_denomination_nat current_outstanding_with_fees) (fixedpoint_to_raw current_imbalance_index))
    (mul_int_int kit_scaling_factor_int (fixedpoint_to_raw last_imbalance_index))

(** Update the checker's parameters, given (a) the current timestamp
    (Tezos.now), (b) the current index (the median of the oracles right now),
    and (c) the current price of kit in tok. *)
let parameters_touch
    (current_index: fixedpoint)
    (current_kit_in_tok: ratio)
    (parameters: parameters)
  : kit * parameters =
  let
    { q = parameters_q;
      index = _parameters_index; (* unused. Always set to the new one. *)
      protected_index = parameters_protected_index;
      target = parameters_target;
      drift_derivative = parameters_drift_derivative;
      drift = parameters_drift;
      burrow_fee_index = parameters_burrow_fee_index;
      imbalance_index = parameters_imbalance_index;
      outstanding_kit = parameters_outstanding_kit;
      circulating_kit = parameters_circulating_kit;
      last_touched = parameters_last_touched;
    } = parameters in

  (* Calculate the number of seconds elapsed. *)
  let now = Tezos.get_now () in
  let duration_in_seconds = sub_timestamp_timestamp now parameters_last_touched in


  (* Update the indices *)
  let current_burrow_fee_index =
    compute_current_burrow_fee_index parameters_burrow_fee_index duration_in_seconds in
  let current_imbalance_index =
    compute_current_imbalance_index parameters_outstanding_kit parameters_circulating_kit parameters_imbalance_index duration_in_seconds in

  (* Calculate all parameter updates and accrual to cfmm. *)
  let current_protected_index =
    compute_current_protected_index parameters_protected_index current_index duration_in_seconds in
  let current_drift_derivative =
    compute_drift_derivative parameters_target in
  let current_drift =
    compute_current_drift parameters_drift parameters_drift_derivative current_drift_derivative duration_in_seconds in
  let current_q =
    compute_current_q parameters_q parameters_drift parameters_drift_derivative current_drift_derivative duration_in_seconds in
  let current_target =
    compute_current_target current_q current_index current_kit_in_tok in
  let current_outstanding_with_fees =
    compute_current_outstanding_with_fees parameters_outstanding_kit parameters_burrow_fee_index current_burrow_fee_index in
  let accrual_to_cfmm = kit_sub current_outstanding_with_fees parameters_outstanding_kit in (* NOTE: can this be negative? *)
  let current_outstanding_kit =
    compute_current_outstanding_kit current_outstanding_with_fees parameters_imbalance_index current_imbalance_index in
  let current_circulating_kit =
    kit_add parameters_circulating_kit accrual_to_cfmm in

  (* Update all values *)
  ( accrual_to_cfmm
  , {
    index = current_index;
    protected_index = current_protected_index;
    target = current_target;
    drift = current_drift;
    drift_derivative = current_drift_derivative;
    q = current_q;
    burrow_fee_index = current_burrow_fee_index;
    imbalance_index = current_imbalance_index;
    outstanding_kit = current_outstanding_kit;
    circulating_kit = current_circulating_kit;
    last_touched = Tezos.get_now ();
  }
  )

(** Add some kit to the total amount of kit in circulation. *)
[@inline] let add_circulating_kit (parameters: parameters) (kit: kit) : parameters =
  { parameters with circulating_kit = kit_add parameters.circulating_kit kit; }

(** Remove some kit from the total amount of kit in circulation. *)
[@inline] let remove_circulating_kit (parameters: parameters) (kit: kit) : parameters =

  { parameters with circulating_kit = kit_sub parameters.circulating_kit kit; }

(** Add some kit to the total amount of kit required to close all burrows and
    the kit in circulation. This is the case when a burrow owner mints kit. *)
[@inline] let add_outstanding_and_circulating_kit (parameters: parameters) (kit: kit) : parameters =
  { parameters with
    outstanding_kit = kit_add parameters.outstanding_kit kit;
    circulating_kit = kit_add parameters.circulating_kit kit;
  }

(** Remove some kit from the total amount of kit required to close all burrows
    and the kit in circulation. This is the case when a burrow owner burns kit. *)
[@inline] let remove_outstanding_and_circulating_kit
    (parameters: parameters)
    (outstanding_to_remove: kit)
    (circulating_to_remove: kit)
  : parameters =


  (* It is currently unclear whether the way we approximate the total
   * outstanding kit is an over- or an under- approximation of the real total
   * outstanding kit (i.e., the sum of b.outstanding_kit for every burrow b).
   * As illustrated in #209 (whose source was another issue, but still), if we
   * are not careful when reducing our approximation of the total outstanding
   * kit checker could potentially get stuck and become untouchable. To avoid
   * that, we use zero as an absorbing bound for the approximation of the total
   * kit outstanding. If anything, by doing so the gap between the real value
   * and the approximation decreases (however, this catch-all could hide other
   * bugs). *)
  let outstanding_to_remove =
    kit_min parameters.outstanding_kit outstanding_to_remove in
  { parameters with
    outstanding_kit = kit_sub parameters.outstanding_kit outstanding_to_remove;
    circulating_kit = kit_sub parameters.circulating_kit circulating_to_remove;
  }
