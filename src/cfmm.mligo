(* INDEX *)

#include "./cfmmTypes.mligo"
#include "./error.mligo"
#include "./constants.mligo"
#import "./common.mligo" "Common"
#import "./ctok.mligo" "Ctok"
#import "./kit.mligo" "Kit"
#import "./fixedPoint.mligo" "Fixedpoint"

(* The general concept of cfmm is that you have quantity a of an asset A
 * and b of an asset B and you process buy and sell requests by maintaining
 * the product a * b constant. So if someone wants to sell a quantity da of
 * asset A to the contract, the balance would become (a + da) so you can
 * give that person a quantity db of asset B in exchange such that (a +
 * da)(b - db) = a * b. Solving for db gives db  = da * b / (a + da). We
 * can rewrite this as db = da * (b / a) * (a / (a + da)) where (b / a)
 * represents the  "price" before the order and a / (a + da)  represents
 * the "slippage". Indeed, a property of cfmm is that with arbitrageurs
 * around, the ratio (a / b) gives you the market price of A in terms of B.
 *
 * On top of that, we can add some fees of 0.2 cNp. So the equation becomes
 * something like db = da * b / (a + da) * (1 - 0.2/100) (note that this
 * formula is a first-order approximation in the sense that two orders of size
 * da / 2 will give you a better price than one order of size da, but the
 * difference is far smaller than typical fees or any amount we care about.
*)
(* Check out dexter for technical details:
     https://gitlab.com/camlcase-dev/dexter/-/blob/master/ligo/dexter.ligo
     https://gitlab.com/camlcase-dev/dexter/-/blob/master/docs/dexter-informal-specification.md
*)

(* When the cfmm is uninitialized, we should not be able to query prices
 * and/or do other things. George: I assume that the only thing we should allow
 * is adding liquidity, to kick things off. I would also like to assume that
 * all the conditions below should be either all true or all false, but the
 * implementation of remove_liquidity currently allows liquidity to reach zero.
 * *)
[@inline] let cfmm_assert_initialized (u: cfmm) : cfmm =
  u

(** Compute the price of kit in ctok (ratio of ctok and kit in the cfmm
    contract), as it was at the end of the last block. This is to be used when
    required for the calculation of the drift derivative instead of up-to-date
    kit_in_ctok, because it is a little harder to manipulate. *)
[@inline] let cfmm_kit_in_ctok_in_prev_block (cfmm: cfmm) : Common.ratio =
  let cfmm = cfmm_assert_initialized cfmm in
  cfmm.kit_in_ctok_in_prev_block

(* Update the kit_in_ctok cached and last_level, if we just entered a new block.
 * This should be called before we make any changes to the contract so that we
 * don't lose the last kit_in_ctok at the end of the last block. George: Note
 * that this is not the previous block, but the last block in which the cfmm
 * contract was touched. *)
let cfmm_sync_last_observed (cfmm: cfmm) : cfmm =
  let level = Tezos.get_level () in
  if level <= cfmm.last_level then
    (* do nothing if it's been touched already in this block *)
    cfmm
  else
    { cfmm with
      kit_in_ctok_in_prev_block =
        Common.make_ratio
          ((Ctok.ctok_to_denomination_nat cfmm.ctok) * Kit.kit_scaling_factor_int)
          ((Kit.kit_to_denomination_nat cfmm.kit) * Ctok.ctok_scaling_factor_int);
      last_level = level;
    }

(** Compute the maximum [min_kit_expected] for [cfmm_buy_kit] to succeed. *)
let cfmm_view_min_kit_expected_buy_kit
    (cfmm: cfmm)
    (_target: Fixedpoint.fixedpoint)
    (ctok_amount: Ctok.ctok)
  : (Kit.kit (* min_kit_expected *) * cfmm) =
  let cfmm = cfmm_sync_last_observed cfmm in
  let cfmm = cfmm_assert_initialized cfmm in
  if (Ctok.eq_ctok_ctok ctok_amount Ctok.ctok_zero) then
    (failwith error_BuyKitNoCtokGiven : (Kit.kit * cfmm))
  else
    (* db = da * (b / a) * (a / (a + da)) * (1 - fee) or
     * db = da * b / (a + da) * (1 - fee) *)
    let { num = num_uf; den = den_uf; } =
      let { num = num_uf; den = den_uf; } = cfmm_fee in
      { num = den_uf - num_uf; den = den_uf; } (* 1 - cfmm_fee *)
    in
    let new_cfmm_ctok = Ctok.ctok_add cfmm.ctok ctok_amount in
    let numerator =
        (Ctok.ctok_to_denomination_nat ctok_amount)
        *
        ((Kit.kit_to_denomination_nat cfmm.kit) * num_uf) in
    let denominator =
        Kit.kit_scaling_factor_int
        *
        ((Ctok.ctok_to_denomination_nat new_cfmm_ctok) * den_uf) in
    let bought_kit = Kit.kit_of_fraction_floor numerator denominator in
    (* Due to (a) the constant-factor calculation (which means that to deplete
     * the one amount the other would in effect have to become infinite), (b)
     * the fact that checker owns 1mu of each token, and (c) the fact that we
     * always floor in our calculations, it should be impossible to trigger the
     * following assertion. *)
    ( bought_kit,
      { cfmm with
        kit = Kit.kit_sub cfmm.kit bought_kit;
        ctok = new_cfmm_ctok;
      }
    )

(** Buy some kit from the cfmm contract by providing some ctok. Fail if the
    desired amount of kit cannot be bought or if the deadline has passed. *)
let cfmm_buy_kit
    (cfmm: cfmm)
    (target: Fixedpoint.fixedpoint)
    (ctok_amount: Ctok.ctok)
    (min_kit_expected: Kit.kit)
    (deadline: timestamp)
  : (Kit.kit * cfmm) =
  if (Ctok.eq_ctok_ctok ctok_amount Ctok.ctok_zero) then
    (failwith error_BuyKitNoCtokGiven : (Kit.kit * cfmm))
  else if (Tezos.get_now ()) >= deadline then
    (failwith error_CfmmTooLate : (Kit.kit * cfmm))
  else if (Kit.eq_kit_kit min_kit_expected Kit.kit_zero) then
    (failwith error_BuyKitTooLowExpectedKit : (Kit.kit * cfmm))
  else
    let (bought_kit, cfmm) = cfmm_view_min_kit_expected_buy_kit cfmm target ctok_amount in
    if bought_kit < min_kit_expected then
      (failwith error_BuyKitPriceFailure : (Kit.kit * cfmm))
    else
      (bought_kit, cfmm)

(** Compute the maximum [min_ctok_expected] for [cfmm_sell_kit] to succeed. *)
let cfmm_view_min_ctok_expected_cfmm_sell_kit
    (cfmm: cfmm)
    (_target: Fixedpoint.fixedpoint)
    (kit_amount: Kit.kit)
  : (Ctok.ctok * cfmm) =
  let cfmm = cfmm_sync_last_observed cfmm in
  let cfmm = cfmm_assert_initialized cfmm in
  if kit_amount = Kit.kit_zero then
    (failwith error_SellKitNoKitGiven : (Ctok.ctok * cfmm))
  else
    (* db = da * (b / a) * (a / (a + da)) * (1 - fee) or
     * db = da * b / (a + da) * (1 - fee) *)
    let { num = num_uf; den = den_uf; } =
      let { num = num_uf; den = den_uf; } = cfmm_fee in
      { num = den_uf - num_uf; den = den_uf; } (* 1 - cfmm_fee *)
    in
    let new_cfmm_kit = Kit.kit_add cfmm.kit kit_amount in
    let numerator =
        (Kit.kit_to_denomination_nat kit_amount)
        *
        ((Ctok.ctok_to_denomination_nat cfmm.ctok) * num_uf) in
    let denominator =
        Ctok.ctok_scaling_factor_int
        *
        ((Kit.kit_to_denomination_nat new_cfmm_kit) * den_uf) in
    let bought_ctok = Ctok.ctok_of_fraction_floor numerator denominator in

    (* Due to (a) the constant-factor calculation (which means that to deplete
     * the one amount the other would in effect have to become infinite), (b)
     * the fact that checker owns 1mu of each token, and (c) the fact that we
     * always floor in our calculations, it should be impossible to trigger the
     * following assertion. *)

    ( bought_ctok,
      { cfmm with
        kit = new_cfmm_kit;
        ctok = Ctok.ctok_sub cfmm.ctok bought_ctok;
      }
    )

(** Sell some kit to the cfmm contract. Fail if the desired amount of ctok
    cannot be bought or if the deadline has passed. *)
let cfmm_sell_kit
    (cfmm: cfmm)
    (target: Fixedpoint.fixedpoint)
    (kit_amount: Kit.kit)
    (min_ctok_expected: Ctok.ctok)
    (deadline: timestamp)
  : (Ctok.ctok * cfmm) =
  if (kit_amount = Kit.kit_zero) then
    (failwith error_SellKitNoKitGiven : (Ctok.ctok * cfmm))
  else if (Tezos.get_now ()) >= deadline then
    (failwith error_CfmmTooLate : (Ctok.ctok * cfmm))
  else if (Ctok.eq_ctok_ctok min_ctok_expected Ctok.ctok_zero) then
    (failwith error_SellKitTooLowExpectedCtok : (Ctok.ctok * cfmm))
  else
    let (bought_ctok, cfmm) = cfmm_view_min_ctok_expected_cfmm_sell_kit cfmm target kit_amount in
    if bought_ctok < min_ctok_expected then
      (failwith error_SellKitPriceFailure : (Ctok.ctok * cfmm))
    else
      (bought_ctok, cfmm)

(** Compute the minimum [max_kit_deposited] and the maximum [min_lqt_minted]
    for [cfmm_add_liquidity] to succeed. *)
let cfmm_view_max_kit_deposited_min_lqt_minted_cfmm_add_liquidity
    (cfmm: cfmm)
    (ctok_amount: Ctok.ctok)
  : (Lqt.lqt * Kit.kit * cfmm) =
  let cfmm = cfmm_sync_last_observed cfmm in
  let cfmm = cfmm_assert_initialized cfmm in
  if Ctok.eq_ctok_ctok ctok_amount Ctok.ctok_zero then
    (failwith error_AddLiquidityNoCtokGiven : (Lqt.lqt * Kit.kit * cfmm))
  else
    let cfmm_ctok = Ctok.ctok_to_denomination_nat cfmm.ctok in
    let lqt_minted =
      Lqt.lqt_of_fraction_floor
        ((Lqt.lqt_to_denomination_int cfmm.lqt) * (Ctok.ctok_to_denomination_nat ctok_amount))
        (Lqt.lqt_scaling_factor_int * cfmm_ctok) in
    let kit_deposited =
      Kit.kit_of_fraction_ceil
        ((Kit.kit_to_denomination_int cfmm.kit) * (Ctok.ctok_to_denomination_nat ctok_amount))
        (Kit.kit_scaling_factor_int * cfmm_ctok) in
    (* Since (a) ctok_amount > 0, (b) cfmm.kit > 0, and (c) we ceil when
     * computing kit_deposited, it should be impossible to trigger the
     * following assertion. *)

    ( lqt_minted,
      kit_deposited,
      { cfmm with
        kit = Kit.kit_add cfmm.kit kit_deposited;
        ctok = Ctok.ctok_add cfmm.ctok ctok_amount;
        lqt = Lqt.lqt_add cfmm.lqt lqt_minted;
      }
    )

(** Buy some liquidity from the cfmm contract, by giving it some ctok and
    some kit. If the given amounts does not have the right ratio, we
    liquidate all the ctok given and as much kit as we can with the right
    ratio, and return the leftovers, along with the liquidity tokens. *)
(* But where do the assets in cfmm come from? Liquidity providers, or
 * "LP" deposit can deposit a quantity la and lb of assets A and B in the
 * same proportion as the contract la / lb = a / b . Assuming there are n
 * "liquidity tokens" extant, they receive m = floor(n la / a) tokens and
 * there are now m +n liquidity tokens extant. They can redeem then at
 * anytime for a fraction of the assets A and B. The reason to do this in
 * cfmm is that usage of cfmm costs 0.3%, and that ultimately can
 * grow the balance of the assets in the contract. An additional reason
 * to do it in checker is that the kit balance of the cfmm contract is
 * continuously credited with the burrow fee taken from burrow holders. *)
let cfmm_add_liquidity
    (cfmm: cfmm)
    (ctok_amount: Ctok.ctok)
    (max_kit_deposited: Kit.kit)
    (min_lqt_minted: Lqt.lqt)
    (deadline: timestamp)
  : (Lqt.lqt * Kit.kit * cfmm) =
  if (Tezos.get_now ()) >= deadline then
    failwith error_CfmmTooLate
  else if Ctok.eq_ctok_ctok ctok_amount Ctok.ctok_zero then
    failwith error_AddLiquidityNoCtokGiven
  else if max_kit_deposited = Kit.kit_zero then
    failwith error_AddLiquidityNoKitGiven
  else if min_lqt_minted = Lqt.lqt_zero then
    failwith error_AddLiquidityNoLiquidityToBeAdded
  else
    let (lqt_minted, kit_deposited, cfmm) =
      cfmm_view_max_kit_deposited_min_lqt_minted_cfmm_add_liquidity cfmm ctok_amount in
    if lqt_minted < min_lqt_minted then
      failwith error_AddLiquidityTooLowLiquidityMinted
    else if max_kit_deposited < kit_deposited then
      failwith error_AddLiquidityTooMuchKitRequired
    else
      let kit_to_return = Kit.kit_sub max_kit_deposited kit_deposited in
      (* EXPECTED PROPERTY: Kit.kit_to_return + final_cfmm_kit = max_kit_deposited + initial_cfmm_kit
       * which follows from the definitions:
       *  kit_to_return  = max_kit_deposited - kit_deposited
       *  final_cfmm_kit = initial_cfmm_kit  + kit_deposited
      *)
      (lqt_minted, kit_to_return, cfmm)

(** Compute the maximum [min_ctok_withdrawn] and the minimum
    [min_kit_withdrawn] for [cfmm_remove_liquidity] to succeed. *)
let cfmm_view_min_ctok_withdrawn_min_kit_withdrawn_cfmm_remove_liquidity
    (cfmm: cfmm)
    (lqt_burned: Lqt.lqt)
  : (Ctok.ctok * Kit.kit * cfmm) =
  let cfmm = cfmm_sync_last_observed cfmm in
  let cfmm = cfmm_assert_initialized cfmm in
  if lqt_burned = Lqt.lqt_zero then
    failwith error_RemoveLiquidityNoLiquidityBurned
  else if lqt_burned >= cfmm.lqt then
    failwith error_RemoveLiquidityTooMuchLiquidityWithdrawn
  else
    let ctok_withdrawn =
      Ctok.ctok_of_fraction_floor
        ((Ctok.ctok_to_denomination_nat cfmm.ctok) * (Lqt.lqt_to_denomination_int lqt_burned))
        (Ctok.ctok_scaling_factor_int * (Lqt.lqt_to_denomination_nat cfmm.lqt))
    in
    let kit_withdrawn =
      Kit.kit_of_fraction_floor
        ((Kit.kit_to_denomination_int cfmm.kit) * (Lqt.lqt_to_denomination_nat lqt_burned))
        (Kit.kit_scaling_factor_int * (Lqt.lqt_to_denomination_nat cfmm.lqt))
    in
    (* Since (a) 0 < lqt_burned < cfmm.lqt, and (b) we floor for both the kit
     * and the ctok withdrawn, it should be impossible to trigger the following
     * assertions. *)
    let remaining_ctok = Ctok.ctok_sub cfmm.ctok ctok_withdrawn in
    let remaining_lqt = Lqt.lqt_sub cfmm.lqt lqt_burned in
    let remaining_kit = Kit.kit_sub cfmm.kit kit_withdrawn in
    let updated = { cfmm with
                    ctok = remaining_ctok;
                    kit = remaining_kit;
                    lqt = remaining_lqt } in
    (ctok_withdrawn, kit_withdrawn, updated)

(** Sell some liquidity to the cfmm contract. Selling liquidity always
    succeeds, but might leave the contract without ctok and kit if everybody
    sells their liquidity. I think it is unlikely to happen, since the last
    liquidity holders wouldn't want to lose the burrow fees. *)
(* Selling liquidity always succeeds, but might leave the contract
 * without ctok and kit if everybody sells their liquidity. I think
 * it is unlikely to happen, since the last liquidity holders wouldn't
 * want to lose the burrow fees. *)
let cfmm_remove_liquidity
    (cfmm: cfmm)
    (lqt_burned: Lqt.lqt)
    (min_ctok_withdrawn: Ctok.ctok)
    (min_kit_withdrawn: Kit.kit)
    (deadline: timestamp)
  : (Ctok.ctok * Kit.kit * cfmm) =
  if (Tezos.get_now ()) >= deadline then
    failwith error_CfmmTooLate
  else if lqt_burned = Lqt.lqt_zero then
    failwith error_RemoveLiquidityNoLiquidityBurned
  else if lqt_burned >= cfmm.lqt then
    failwith error_RemoveLiquidityTooMuchLiquidityWithdrawn
  else if Ctok.eq_ctok_ctok min_ctok_withdrawn Ctok.ctok_zero then
    failwith error_RemoveLiquidityNoCtokWithdrawnExpected
  else if min_kit_withdrawn = Kit.kit_zero then
    failwith error_RemoveLiquidityNoKitWithdrawnExpected
  else
    let (ctok_withdrawn, kit_withdrawn, cfmm) =
      cfmm_view_min_ctok_withdrawn_min_kit_withdrawn_cfmm_remove_liquidity cfmm lqt_burned in
    if ctok_withdrawn < min_ctok_withdrawn then
      failwith error_RemoveLiquidityCantWithdrawEnoughCtok
    else if kit_withdrawn < min_kit_withdrawn then
      failwith error_RemoveLiquidityCantWithdrawEnoughKit
    else
      (ctok_withdrawn, kit_withdrawn, cfmm)

(** Add accrued burrowing fees to the cfmm contract. *)
let cfmm_add_accrued_kit (cfmm: cfmm) (accrual: Kit.kit) : cfmm =
  let cfmm = cfmm_sync_last_observed cfmm in
  { cfmm with kit = Kit.kit_add cfmm.kit accrual }
