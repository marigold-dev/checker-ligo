#import "./common.mligo" "Common"
#import "./fixedPoint.mligo" "Fixedpoint"

(** Given the current target, calculate the rate of change of the drift (drift
    derivative). That's how the following calculations came to be:

    let [X = log (p_t)] be the "measure of imbalance". The original doc gave:
    {[
      d_t' = 0                             if 0       <= |X| < 0.5 cNp
      d_t' = sign(X) * 0.01 cNp / day^2    if 0.5 cNp <= |X| <   5 cNp
      d_t' = sign(X) * 0.05 cNp / day^2    if   5 cNp <= |X| < infinity
    ]}

    1. Inline the numbers: cNp ~= 1/100, day ~= 24 * 60 * 60 = 86400 seconds
    {[
      d_t' = 0                             if 0     <= |X| < 0.005
      d_t' = sign(X) * 0.0001 / 86400^2    if 0.005 <= |X| < 0.05
      d_t' = sign(X) * 0.0005 / 86400^2    if 0.05  <= |X| < infinity
    ]}

    2. Remove absolute values
    {[
      d_t' =  0                   if -0.005 <  X <  0.005
      d_t' = +0.0001 / 86400^2    if +0.005 <= X < +0.05
      d_t' = -0.0001 / 86400^2    if -0.005 >= X > -0.05
      d_t' = +0.0005 / 86400^2    if +0.05  <= X < +infinity
      d_t' = -0.0005 / 86400^2    if -0.05  >= X > -infinity
    ]}

    3. Exponentiate the inequalities
    {[
      d_t' =  0                   if exp(-0.005) <  p_t < exp(+0.005)
      d_t' = +0.0001 / 86400^2    if exp(+0.005) <= p_t < exp(+0.05)
      d_t' = -0.0001 / 86400^2    if exp(-0.005) >= p_t > exp(-0.05)
      d_t' = +0.0005 / 86400^2    if exp(+0.05)  <= p_t < +infinity
      d_t' = -0.0005 / 86400^2    if exp(-0.05)  >= p_t > -infinity
    ]}
*)
[@inline] let compute_drift_derivative (target : Fixedpoint.t) : Fixedpoint.t =
  (* Curve parameters

     The drift derivative can take one of 5 distinct values: 0, +/-0.01 cNp/day,
     and +/-0.05 cNp/day. We calculate those statically thus as follows:
     {[
       low_acceleration  = 0.01/100 * (86400 * 86400) = 1/74649600000000 =  247111 in fixedpoint
       high_acceleration = 0.05/100 * (86400 * 86400) = 5/74649600000000 = 1235555 in fixedpoint
     ]}
  *)
  [@inline] let target_low_bracket : Common.ratio = Common.make_ratio ((5)) ((1000)) in
  [@inline] let target_high_bracket : Common.ratio = Common.make_ratio ((5)) ((100)) in
  [@inline] let low_positive_acceleration : Fixedpoint.t = Fixedpoint.fixedpoint_of_raw ((247111)) in
  [@inline] let low_negative_acceleration : Fixedpoint.t = Fixedpoint.fixedpoint_of_raw ((-247111)) in
  [@inline] let high_positive_acceleration : Fixedpoint.t = Fixedpoint.fixedpoint_of_raw ((1235555)) in
  [@inline] let high_negative_acceleration : Fixedpoint.t = Fixedpoint.fixedpoint_of_raw ((-1235555)) in

  let { num = num_tlb; den = den_tlb; } = target_low_bracket in
  let { num = num_thb; den = den_thb; } = target_high_bracket in
  let target = Fixedpoint.fixedpoint_to_raw target in

  let mul_target_tlb = target * den_tlb in
  let mul_sub_den_tlb_num_tlb_sf = (den_tlb - num_tlb) * Fixedpoint.fixedpoint_scaling_factor_int in
  let mul_add_den_tlb_num_tlb_sf = (den_tlb + num_tlb) * Fixedpoint.fixedpoint_scaling_factor_int in

  let mul_target_thb = target * den_thb in
  let mul_sub_den_thb_num_thb_sf = (den_thb - num_thb) * Fixedpoint.fixedpoint_scaling_factor_int in
  let mul_add_den_thb_num_thb_sf = (den_thb + num_thb) * Fixedpoint.fixedpoint_scaling_factor_int in

  if (mul_sub_den_tlb_num_tlb_sf < mul_target_tlb)
  && (mul_target_tlb < mul_add_den_tlb_num_tlb_sf) then
    Fixedpoint.fixedpoint_zero (* no acceleration (0) *)
  else if (mul_sub_den_thb_num_thb_sf < mul_target_thb)
       && (mul_target_tlb <= mul_sub_den_tlb_num_tlb_sf) then
    low_negative_acceleration
  else if (mul_add_den_thb_num_thb_sf > mul_target_thb)
       && (mul_target_tlb >= mul_add_den_tlb_num_tlb_sf) then
    low_positive_acceleration
  else if mul_target_thb <= mul_sub_den_thb_num_thb_sf then
    high_negative_acceleration
  else
    high_positive_acceleration
