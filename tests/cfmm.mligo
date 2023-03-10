#import "../breathalyzer/lib/lib.mligo" "Breath"
#import "../src/common.mligo" "Common"
#import "../src/cfmm.mligo" "CFMM"
#import "../src/fixedPoint.mligo" "FixedPoint"
#import "../src/kit.mligo" "Kit"
#import "../src/lqt.mligo" "Lqt"
#import "./ratio.mligo" "Ratio"
#import "../src/ctok.mligo" "Ctok"

type cfmm = {
    ctok: Ctok.ctok;
    kit: Kit.kit;
    lqt: Lqt.lqt;
    kit_in_ctok_in_prev_block: Common.ratio (* [@printer pp_ratio] *);
    last_level: nat;
  }

let cfmm0 =
  let ctok = Ctok.ctok_of_denomination 100n in
  let kit = Kit.kit_of_denomination 100n in
  {
    ctok = ctok;
    kit = kit;
    lqt = Lqt.lqt_of_denomination 100n;
    kit_in_ctok_in_prev_block =
      Common.make_ratio (ctok * Kit.kit_scaling_factor_int) (kit * Ctok.ctok_scaling_factor_int);
    last_level = 1n;
  }

let dummy_target : FixedPoint.fixedpoint =
  FixedPoint.fixedpoint_sub FixedPoint.fixedpoint_zero FixedPoint.fixedpoint_one

(* TODO: various amounts? *)
let ten_kits : Kit.kit = 10n
let ten_ctoks : Ctok.ctok = 10n

let test = ten_kits + ten_ctoks

let min_kit_expected : Kit.kit = 1n

let min_ctok_expected : Ctok.ctok = 1n

(* Compute the current price of kit in ctok, as estimated using the ratio of ctok and kit
 * currently in the cfmm contract. *)
let cfmm_kit_in_ctok (u: cfmm) =
  Ratio.div_ratio (Ratio.ratio_of_nat u.ctok) (Ratio.of_kit u.kit)

(* Compute the current product of kit and ctok, using the current contents of the cfmm
 * contract. *)
let cfmm_kit_times_ctok (u: cfmm) =
  Ratio.mul_ratio (Ratio.ratio_of_nat u.ctok) (Ratio.of_kit u.kit)

let suite = [
  Breath.Model.case
    "buy_kit_increases_price"
    ""
    (fun _ ->
      let deadline = Tezos.get_now () + 30 in
      let (_, new_cfmm) =
        CFMM.cfmm_buy_kit cfmm0 dummy_target ten_ctoks min_kit_expected deadline
      in
      Breath.Assert.is_true
        ""
        (Ratio.gt_ratio_ratio (cfmm_kit_in_ctok new_cfmm) (cfmm_kit_in_ctok cfmm0)));

  Breath.Model.case
    "buy_kit_increases_product"
    ""
    (fun _ ->
      let deadline = Tezos.get_now () + 30 in
      let (_, new_cfmm) =
        CFMM.cfmm_buy_kit cfmm0 dummy_target ten_ctoks min_kit_expected deadline
      in
      Breath.Assert.is_true
        ""
        (Ratio.gt_ratio_ratio (cfmm_kit_times_ctok new_cfmm) (cfmm_kit_times_ctok cfmm0)));

  Breath.Model.case
    "buy_kit_does_not_affect_liquidity"
    ""
    (fun _ ->
      let deadline = Tezos.get_now () + 30 in
      let (_, new_cfmm) =
        CFMM.cfmm_buy_kit cfmm0 dummy_target ten_ctoks min_kit_expected deadline
      in
      Breath.Assert.is_equal
        ""
        (new_cfmm.lqt) (cfmm0.lqt));

  Breath.Model.case
    "buy_kit_respects_min_kit_expected"
    ""
    (fun _ ->
      let deadline = Tezos.get_now () + 30 in
      let (bought_kit, _) =
        CFMM.cfmm_buy_kit cfmm0 dummy_target ten_ctoks min_kit_expected deadline
      in
      Breath.Assert.is_true
        ""
        (bought_kit >= min_kit_expected));

  Breath.Model.case
    "buy_kit_preserves_kit"
    ""
    (fun _ ->
      let deadline = Tezos.get_now () + 30 in
      let (bought_kit, new_cfmm) =
        CFMM.cfmm_buy_kit cfmm0 dummy_target ten_ctoks min_kit_expected deadline
      in
      Breath.Assert.is_true
        ""
        (cfmm0.kit = Kit.kit_add new_cfmm.kit bought_kit));

  Breath.Model.case
    "buy_kit_preserves_ctok"
    ""
    (fun _ ->
      let deadline = Tezos.get_now () + 30 in
      let (_, new_cfmm) =
        CFMM.cfmm_buy_kit cfmm0 dummy_target ten_ctoks min_kit_expected deadline
      in
      Breath.Assert.is_true
        ""
        (new_cfmm.ctok = Ctok.ctok_add cfmm0.ctok ten_ctoks));

  Breath.Model.case
    "sell_kit_decreases_price"
    ""
    (fun _ ->
      let deadline = Tezos.get_now () + 30 in
      let (_, new_cfmm) =
        CFMM.cfmm_sell_kit cfmm0 dummy_target ten_kits min_ctok_expected deadline
      in
      Breath.Assert.is_true
        ""
        (Ratio.lt_ratio_ratio (cfmm_kit_in_ctok new_cfmm) (cfmm_kit_in_ctok cfmm0)));

  Breath.Model.case
    "sell_kit_increases_product"
    ""
    (fun _ ->
      let deadline = Tezos.get_now () + 30 in
      let (_, new_cfmm) =
        CFMM.cfmm_sell_kit cfmm0 dummy_target ten_kits min_ctok_expected deadline
      in
      Breath.Assert.is_true
        ""
        (Ratio.gt_ratio_ratio (cfmm_kit_times_ctok new_cfmm) (cfmm_kit_times_ctok cfmm0)));

  Breath.Model.case
    "sell_kit_does_not_affect_liquidity"
    ""
    (fun _ ->
      let deadline = Tezos.get_now () + 30 in
      let (_, new_cfmm) =
        CFMM.cfmm_sell_kit cfmm0 dummy_target ten_kits min_ctok_expected deadline
      in
      Breath.Assert.is_equal
        ""
        (new_cfmm.lqt) (cfmm0.lqt));

  Breath.Model.case
    "sell_kit_respects_min_ctok_expected"
    ""
    (fun _ ->
      let deadline = Tezos.get_now () + 30 in
      let (bought, new_cfmm) =
        CFMM.cfmm_sell_kit cfmm0 dummy_target ten_kits min_ctok_expected deadline
      in
      Breath.Assert.is_true
        ""
        (bought >= min_ctok_expected));

  Breath.Model.case
    "sell_kit_preserves_kit"
    ""
    (fun _ ->
      let deadline = Tezos.get_now () + 30 in
      let (_, new_cfmm) =
        CFMM.cfmm_sell_kit cfmm0 dummy_target ten_kits min_ctok_expected deadline
      in
      Breath.Assert.is_true
        ""
        (new_cfmm.kit = Kit.kit_add cfmm0.kit ten_kits));

  Breath.Model.case
    "sell_kit_preserves_ctok"
    ""
    (fun _ ->
      let deadline = Tezos.get_now () + 30 in
      let (ctok_bought, new_cfmm) =
        CFMM.cfmm_sell_kit cfmm0 dummy_target ten_kits min_ctok_expected deadline
      in
      Breath.Assert.is_true
        ""
        (cfmm0.ctok = Ctok.ctok_add new_cfmm.ctok ctok_bought));
]

