#import "../breathalyzer/lib/lib.mligo" "Breath"
#import "../math-lib-cameligo/core/math.mligo" "Math"
#import "../src/tokenMetadata.mligo" "Metadata"
#import "../src/kit.mligo" "Kit"
#import "../src/fixedPoint.mligo" "Fixedpoint"
#import "./ratio.mligo" "Ratio"

let suite = [

  Breath.Model.case
    "Kit arithmetic"
    ""
    (fun _ ->
      let actual_scaling_factor = Math.power (10n, Metadata.kit_decimal_digits) in
      Breath.Result.reduce [
        Breath.Assert.is_equal "" Kit.kit_scaling_factor_nat actual_scaling_factor;
        Breath.Assert.is_equal "" Kit.kit_scaling_factor_int (int Kit.kit_scaling_factor_nat);
        (* We skip basic arithmetic because it's just testing the nat type *)
        Breath.Assert.is_equal ""
          (Kit.kit_scale 5123467n (Fixedpoint.fixedpoint_of_ratio_floor (Ratio.ratio_of_int 3)))
          15370401n
      ]);

]
