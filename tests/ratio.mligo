#import "../src/common.mligo" "Common"
#import "../src/kit.mligo" "Kit"

let ratio_of_int (i: int) : Common.ratio = { num = i; den = 1}

[@inline] let ratio_of_nat (n: nat) : Common.ratio = ratio_of_int (int n)

let eq_ratio_ratio (x: Common.ratio) (y: Common.ratio) : bool =
  let { num = x_num; den = x_den; } = x in
  let { num = y_num; den = y_den; } = y in
  (x_num * y_den) = (y_num * x_den)

let lt_ratio_ratio (x: Common.ratio) (y: Common.ratio) : bool =
  let { num = x_num; den = x_den; } = x in
  let { num = y_num; den = y_den; } = y in
  x_num * y_den < y_num * x_den

let leq_ratio_ratio (x: Common.ratio) (y: Common.ratio) : bool =
  let { num = x_num; den = x_den; } = x in
  let { num = y_num; den = y_den; } = y in
  x_num * y_den <= y_num * x_den

let geq_ratio_ratio (x: Common.ratio) (y: Common.ratio) : bool = leq_ratio_ratio y x

let gt_ratio_ratio (x: Common.ratio) (y: Common.ratio) : bool = lt_ratio_ratio y x

let mul_ratio (x: Common.ratio) (y: Common.ratio) : Common.ratio =
  let { num = x_num; den = x_den; } = x in
  let { num = y_num; den = y_den; } = y in
  Common.make_ratio
    (x_num * y_num)
    (x_den * y_den)

let div_ratio (x: Common.ratio) (y: Common.ratio) : Common.ratio =
  let { num = y_num; den = y_den; } = y in
  if y_num = 0 then
    failwith "Ratio.div_ratio: division by zero"
  else
    mul_ratio x { num = y_den; den = y_num; }

let of_kit (k: Kit.t) = Common.make_ratio (int k) Kit.kit_scaling_factor_int
