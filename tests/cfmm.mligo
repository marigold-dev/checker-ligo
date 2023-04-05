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

let make_cfmm ctok kit lqt last_level : cfmm =
  {
    ctok = Ctok.ctok_of_denomination ctok;
    kit = Kit.kit_of_denomination kit;
    lqt = Lqt.lqt_of_denomination lqt;
    kit_in_ctok_in_prev_block = Ratio.ratio_of_int 1;
    last_level = last_level
  }

let equal_cfmm (cfmm1 : cfmm) (cfmm2 : cfmm) : bool =
  cfmm1.ctok = cfmm2.ctok
  && cfmm1.kit = cfmm2.kit
  && cfmm1.lqt = cfmm2.lqt
  && cfmm1.last_level = cfmm2.last_level
  && Ratio.eq_ratio_ratio cfmm1.kit_in_ctok_in_prev_block cfmm2.kit_in_ctok_in_prev_block


let cfmm0 =
  let ctok = Ctok.ctok_of_denomination 102n in
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


let deadline = Tezos.get_now () + 30

(* Test that adding liquidity to an initial cfmm produces the expected result *)
let liquidity_test initial_cfmm expected_cfmm (added_liquidity : (Ctok.ctok * Kit.kit * Lqt.lqt))
  (expected_returns : (Kit.kit * Lqt.lqt)) =
  Breath.Model.case
    "liquidity_test_with_values"
    ""
    (fun _ ->
      let (ctok, max_kit, min_lqt) = added_liquidity in
      let (expected_kits, expected_liquidity) = expected_returns in
      let (returned_lqt, returned_kits, new_cfmm) = CFMM.cfmm_add_liquidity initial_cfmm ctok max_kit min_lqt deadline in
      let _ = Breath.Assert.is_equal "" returned_lqt expected_liquidity in
      let _ = Breath.Assert.is_equal "" returned_kits expected_kits in
      Breath.Assert.is_true "" (equal_cfmm new_cfmm expected_cfmm))

let suite = [
  Breath.Model.case
    "buy_kit_increases_price"
    ""
    (fun _ ->
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
      let (bought, _) =
        CFMM.cfmm_sell_kit cfmm0 dummy_target ten_kits min_ctok_expected deadline
      in
      Breath.Assert.is_true
        ""
        (bought >= min_ctok_expected));

  Breath.Model.case
    "sell_kit_preserves_kit"
    ""
    (fun _ ->
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
      let (ctok_bought, new_cfmm) =
        CFMM.cfmm_sell_kit cfmm0 dummy_target ten_kits min_ctok_expected deadline
      in
      Breath.Assert.is_true
        ""
        (cfmm0.ctok = Ctok.ctok_add new_cfmm.ctok ctok_bought));

  Breath.Model.case
    "add_liquidity_might_decrease_price"
    ""
    (fun _ ->
      let ctok_amount = Ctok.ctok_of_denomination 100n in
      let max_kit = Kit.kit_of_denomination 100n in
      let min_lqt = Lqt.lqt_of_denomination 98n in
      let _, _, new_cfmm = CFMM.cfmm_add_liquidity cfmm0 ctok_amount max_kit min_lqt deadline in
      Breath.Assert.is_true
        ""
        (Ratio.leq_ratio_ratio (cfmm_kit_in_ctok new_cfmm) (cfmm_kit_in_ctok cfmm0)));

  Breath.Model.case
    "add_liquidity_increases_product"
    ""
    (fun _ ->
      let ctok_amount = Ctok.ctok_of_denomination 100n in
      let max_kit = Kit.kit_of_denomination 100n in
      let min_lqt = Lqt.lqt_of_denomination 98n in
      let _, _, new_cfmm = CFMM.cfmm_add_liquidity cfmm0 ctok_amount max_kit min_lqt deadline in
      Breath.Assert.is_true
        ""
        (Ratio.gt_ratio_ratio (cfmm_kit_times_ctok new_cfmm) (cfmm_kit_times_ctok cfmm0)));

  Breath.Model.case
    "add_liquidity_increases_liquidity"
    ""
    (fun _ ->
      let ctok_amount = Ctok.ctok_of_denomination 100n in
      let max_kit = Kit.kit_of_denomination 100n in
      let min_lqt = Lqt.lqt_of_denomination 98n in
      let _, _, new_cfmm = CFMM.cfmm_add_liquidity cfmm0 ctok_amount max_kit min_lqt deadline in
      Breath.Assert.is_true
        ""
        (new_cfmm.lqt > cfmm0.lqt));

  Breath.Model.case
    "add_liquidity_returns_less_kits"
    ""
    (fun _ ->
      let ctok_amount = Ctok.ctok_of_denomination 100n in
      let max_kit = Kit.kit_of_denomination 100n in
      let min_lqt = Lqt.lqt_of_denomination 98n in
      let _, kits, _= CFMM.cfmm_add_liquidity cfmm0 ctok_amount max_kit min_lqt deadline in
      Breath.Assert.is_true
        ""
        (kits < max_kit));

  Breath.Model.case
    "add_liquidity_respects_min_lqt"
    ""
    (fun _ ->
      let ctok_amount = Ctok.ctok_of_denomination 100n in
      let max_kit = Kit.kit_of_denomination 100n in
      let min_lqt = Lqt.lqt_of_denomination 98n in
      let lqt_minted, _, _= CFMM.cfmm_add_liquidity cfmm0 ctok_amount max_kit min_lqt deadline in
      Breath.Assert.is_true
        ""
        (lqt_minted >= min_lqt));

  Breath.Model.case
    "add_liquidity_respects_max_kit_deposited"
    ""
    (fun _ ->
      let ctok_amount = Ctok.ctok_of_denomination 100n in
      let max_kit = Kit.kit_of_denomination 100n in
      let min_lqt = Lqt.lqt_of_denomination 98n in
      let _, _, new_cfmm = CFMM.cfmm_add_liquidity cfmm0 ctok_amount max_kit min_lqt deadline in
      Breath.Assert.is_true
        ""
        (new_cfmm.kit <= Kit.kit_add cfmm0.kit max_kit));

  liquidity_test
    (make_cfmm 8336667n 6000000n 1n 0n)
    (make_cfmm 28336667n 20394242n 3n 0n)
    (20000000n, 20000000n, 2n)
    (5605758n, 2n);

  Breath.Model.case
    "remove_liquidity_might_increase_price"
    ""
    (fun _ ->
      let lqt = Lqt.lqt_of_denomination 55n in
      let min_ctok = Ctok.ctok_of_denomination 55n in
      let min_kit = Kit.kit_of_denomination 55n in
      let _, _, new_cfmm = CFMM.cfmm_remove_liquidity cfmm0 lqt min_ctok min_kit deadline in
      Breath.Assert.is_true
        ""
        (Ratio.geq_ratio_ratio (cfmm_kit_in_ctok new_cfmm) (cfmm_kit_in_ctok cfmm0)));

  Breath.Model.case
    "remove_liquidity_decreases_product"
    ""
    (fun _ ->
      let lqt = Lqt.lqt_of_denomination 55n in
      let min_ctok = Ctok.ctok_of_denomination 55n in
      let min_kit = Kit.kit_of_denomination 55n in
      let _, _, new_cfmm = CFMM.cfmm_remove_liquidity cfmm0 lqt min_ctok min_kit deadline in
      Breath.Assert.is_true
        ""
        (Ratio.lt_ratio_ratio (cfmm_kit_times_ctok new_cfmm) (cfmm_kit_times_ctok cfmm0)));

  Breath.Model.case
    "remove_liquidity_decreases_liquidity"
    ""
    (fun _ ->
      let lqt = Lqt.lqt_of_denomination 55n in
      let min_ctok = Ctok.ctok_of_denomination 55n in
      let min_kit = Kit.kit_of_denomination 55n in
      let _, _, new_cfmm = CFMM.cfmm_remove_liquidity cfmm0 lqt min_ctok min_kit deadline in
      Breath.Assert.is_true
        ""
        (new_cfmm.lqt < cfmm0.lqt));

  Breath.Model.case
    "remove_liquidity_returns_more_ctok"
    ""
    (fun _ ->
      let lqt = Lqt.lqt_of_denomination 55n in
      let min_ctok = Ctok.ctok_of_denomination 55n in
      let min_kit = Kit.kit_of_denomination 55n in
      let ctoks, _, _ = CFMM.cfmm_remove_liquidity cfmm0 lqt min_ctok min_kit deadline in
      Breath.Assert.is_true
        ""
        (ctoks >= min_ctok));

  Breath.Model.case
    "remove_liquidity_returns_more_kits"
    ""
    (fun _ ->
      let lqt = Lqt.lqt_of_denomination 55n in
      let min_ctok = Ctok.ctok_of_denomination 55n in
      let min_kit = Kit.kit_of_denomination 55n in
      let _, kits, _= CFMM.cfmm_remove_liquidity cfmm0 lqt min_ctok min_kit deadline in
      Breath.Assert.is_true
        ""
        (kits >= min_kit));

  Breath.Model.case
    "remove_liquidity_respects_lqt"
    ""
    (fun _ ->
      let lqt = Lqt.lqt_of_denomination 55n in
      let min_ctok = Ctok.ctok_of_denomination 55n in
      let min_kit = Kit.kit_of_denomination 55n in
      let _, _, new_cfmm = CFMM.cfmm_remove_liquidity cfmm0 lqt min_ctok min_kit deadline in
      Breath.Assert.is_true
        ""
        (new_cfmm.lqt + lqt = cfmm0.lqt));

  Breath.Model.case
    "remove_liquidity_respects_ctok_limit"
    ""
    (fun _ ->
      let lqt = Lqt.lqt_of_denomination 55n in
      let min_ctok = Ctok.ctok_of_denomination 55n in
      let min_kit = Kit.kit_of_denomination 55n in
      let ctoks, _, _ = CFMM.cfmm_remove_liquidity cfmm0 lqt min_ctok min_kit deadline in
      Breath.Assert.is_true
        ""
        (ctoks <= cfmm0.ctok));

  Breath.Model.case
    "remove_liquidity_respects_kit_limit"
    ""
    (fun _ ->
      let lqt = Lqt.lqt_of_denomination 55n in
      let min_ctok = Ctok.ctok_of_denomination 55n in
      let min_kit = Kit.kit_of_denomination 55n in
      let _, kits, _ = CFMM.cfmm_remove_liquidity cfmm0 lqt min_ctok min_kit deadline in
      Breath.Assert.is_true
        ""
        (kits <= cfmm0.kit));

  (* TODO: test failures *)

]

