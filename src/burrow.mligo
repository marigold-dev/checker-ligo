#import "./fixedPoint.mligo" "Fixedpoint"
#import "./kit.mligo" "Kit"
#import "./tok.mligo" "Tok"
#import "./parameters.mligo" "Parameters"
#import "./common.mligo" "Common"
#import "./constants.mligo" "Constants"
#include "./liquidationAuctionPrimitiveTypes.mligo"
(* open LiquidationAuctionPrimitiveTypes *)

type burrow =
  { (* Whether the creation deposit for the burrow has been paid. If the
     * creation deposit has been paid, the burrow is considered "active" and
     * "closed"/inactive otherwise. Paying the creation deposit re-activates
     * a "closed" burrow. *)
    active : bool;
    (* Address of the contract holding the burrow's collateral. *)
    address: address;
    (* Collateral currently stored in the burrow. *)
    collateral : Tok.t;
    (* Outstanding kit minted out of the burrow. *)
    outstanding_kit : Kit.t;
    (* The imbalance adjustment index observed the last time the burrow was
     * touched. *)
    adjustment_index : Fixedpoint.t;
    (* Collateral that has been sent off to auctions. For all intents and
     * purposes, this collateral can be considered gone, but depending on the
     * outcome of the auctions we expect some kit in return. *)
    collateral_at_auction : Tok.t;
    (* The timestamp checker had the last time the burrow was touched. *)
    last_checker_timestamp : timestamp;
  }
(* [@@deriving show] *)

type liquidation_details =
  { liquidation_reward : Tok.t ;
    collateral_to_auction : Tok.t ;
    burrow_state : burrow;
  }
(* [@@deriving show] *)

type liquidation_type =
  (* partial: some collateral remains in the burrow *)
  | Partial
  (* complete: deplete the collateral *)
  | Complete
  (* complete: deplete the collateral AND the creation deposit *)
  | Close
(* [@@deriving show] *)

type liquidation_result = (liquidation_type * liquidation_details) option
(* [@@deriving show] *)

(* [@@@coverage on] *)

(** Update the outstanding kit, update the adjustment index, and the timestamp. *)
let burrow_touch (p: Parameters.t) (burrow: burrow) : burrow =
  let burrow_out = if p.last_touched = burrow.last_checker_timestamp
    then
      burrow
    else
      let current_adjustment_index = Parameters.compute_adjustment_index p in
      { burrow with
        outstanding_kit =
          Kit.kit_of_fraction_floor
            (
               (Kit.kit_to_denomination_nat burrow.outstanding_kit)
               *
               (Fixedpoint.fixedpoint_to_raw current_adjustment_index)
            )
            (
               Kit.kit_scaling_factor_int
               *
               (Fixedpoint.fixedpoint_to_raw burrow.adjustment_index)
            );
        adjustment_index = current_adjustment_index;
        last_checker_timestamp = p.last_touched;
      }
  in

  burrow_out

[@inline] let burrow_address (b: burrow) : address =
  b.address

(** Computes the total amount of tok associated with a burrow. This includes
  * the collateral, collateral_at_auction, and the creation_deposit if the
  * burrow is active. *)
let burrow_total_associated_tok (b: burrow) : Tok.t =
  Tok.tok_add
    (Tok.tok_add b.collateral b.collateral_at_auction)
    (if b.active then Constants.creation_deposit else Tok.tok_zero)

[@inline] let burrow_collateral_at_auction (b: burrow) : Tok.t =
  b.collateral_at_auction

(** Under-collateralization condition: tok < f * kit * price. *)
[@inline] let undercollateralization_condition (f: Common.ratio) (price: Common.ratio) (tok: Common.ratio) (kit: Common.ratio) : bool =
  let { num = num_f; den = den_f; } = f in
  let { num = num_p; den = den_p; } = price in
  let { num = num_tz; den = den_tz; } = tok in
  let { num = num_kt; den = den_kt; } = kit in
  let lhs =
      (num_tz * den_f)
      *
      (den_kt * den_p) in
  let rhs =
      (num_f * num_kt)
      *
      (den_tz * num_p) in
  lhs < rhs

(** Check whether a burrow is overburrowed. A burrow is overburrowed if
  *
  *   collateral < fminting * kit_outstanding * minting_price
  *
  * The quantity collateral / (fminting * minting_price) we call the burrowing
  * limit (normally kit_outstanding <= burrowing_limit). NOTE: for the
  * purposes of minting/checking overburrowedness, we do not take into
  * account expected kit from pending auctions; for all we know, this could
  * be lost forever.
*)
let burrow_is_overburrowed (p: Parameters.t) (b: burrow) : bool =

  let tok = { num = Tok.tok_to_denomination_int b.collateral; den = Tok.tok_scaling_factor_int; } in
  let kit = { num = Kit.kit_to_denomination_int b.outstanding_kit; den = Kit.kit_scaling_factor_int; } in
  undercollateralization_condition Constants.fminting (Parameters.minting_price p) tok kit

(*  max_kit_outstanding = FLOOR (collateral / (fminting * minting_price)) *)
let burrow_max_mintable_kit (p: Parameters.t) (b: burrow) : Kit.t =

  let { num = num_fm; den = den_fm; } = Constants.fminting in
  let { num = num_mp; den = den_mp; } = Parameters.minting_price p in
  let numerator =
      (Tok.tok_to_denomination_nat b.collateral)
      *
      (den_fm * den_mp) in
  let denominator =
      Tok.tok_scaling_factor_int
      *
      (num_fm * num_mp) in
  Kit.kit_of_fraction_floor numerator denominator

let burrow_return_slice_from_auction
    (slice: liquidation_slice_contents)
    (burrow: burrow)
  : burrow =


  let burrow_out =
    { burrow with
      collateral = tok_add burrow.collateral slice.tok;
      collateral_at_auction = tok_sub burrow.collateral_at_auction slice.tok;
    } in

  burrow_out

let burrow_return_kit_from_auction
    (slice: liquidation_slice_contents)
    (kit: kit)
    (burrow: burrow) : burrow * kit * kit =


  let returned_kit = kit_min burrow.outstanding_kit kit in
  let excess_kit = kit_sub kit returned_kit in

  let burrow_out =
    { burrow with
      outstanding_kit = kit_sub burrow.outstanding_kit returned_kit;
      collateral_at_auction = tok_sub burrow.collateral_at_auction slice.tok;
    } in



  (burrow_out, returned_kit, excess_kit)

let burrow_create (p: Parameters.t) (addr: address) (tok: Tok.t) : burrow =
  if tok < Constants.creation_deposit
  then (failwith error_InsufficientFunds : burrow)
  else
    { active = true;
      address = addr;
      collateral = Tok.tok_sub tok Constants.creation_deposit;
      outstanding_kit = kit_zero;
      adjustment_index = Parameters.compute_adjustment_index p;
      collateral_at_auction = tok_zero;
      last_checker_timestamp = p.last_touched; (* NOTE: If checker is up-to-date, the timestamp should be _now_. *)
    }

(** Add non-negative collateral to a burrow. *)
(* TOKFIX: we need a more generic name (e.g., deposit_collateral) *)
[@inline] let burrow_deposit_collateral (p: Parameters.t) (t: Tok.t) (b: burrow) : burrow =
  let b = burrow_touch p b in
  let burrow_out = { b with collateral = tok_add b.collateral t } in

  burrow_out

(** Withdraw a non-negative amount of collateral from the burrow, as long as
  * this will not overburrow it. *)
(* TOKFIX: we need a more generic name (e.g., withdraw_collateral) *)
let burrow_withdraw_collateral (p: Parameters.t) (t: tok) (b: burrow) : burrow =
  let b = burrow_touch p b in
  let burrow = { b with collateral = tok_sub b.collateral t } in
  let burrow_out = if burrow_is_overburrowed p burrow
    then (failwith error_WithdrawTezFailure : burrow)
    else burrow
  in

  burrow_out

(** Mint a non-negative amount of kits from the burrow, as long as this will
  * not overburrow it *)
let burrow_mint_kit (p: Parameters.t) (kit: Kit.t) (b: burrow) : burrow =
  let b = burrow_touch p b in
  let burrow_out =
    let burrow = { b with outstanding_kit = kit_add b.outstanding_kit kit } in
    if burrow_is_overburrowed p burrow
    then (failwith error_MintKitFailure : burrow)
    else burrow
  in

  burrow_out

(** Deposit/burn a non-negative amount of kit to the burrow. Return the amount
  * of kit burned. *)
[@inline] let burrow_burn_kit (p: Parameters.t) (kit: kit) (b: burrow) : burrow * kit =
  let b = burrow_touch p b in
  let actual_burned = kit_min b.outstanding_kit kit in
  let burrow_out = {b with outstanding_kit = kit_sub b.outstanding_kit actual_burned} in

  (burrow_out, actual_burned)

(** Activate a currently inactive burrow. This operation will fail if either
  * the burrow is already active, or if the amount of tez given is less than
  * the creation deposit. *)
let burrow_activate (p: Parameters.t) (tok: tok) (b: burrow) : burrow =
  let b = burrow_touch p b in
  let burrow_out =
    if Tok.lt_tok_tok tok Constants.creation_deposit then
      (failwith error_InsufficientFunds : burrow)
    else if b.active then
      (failwith error_BurrowIsAlreadyActive : burrow)
    else
      { b with
        active = true;
        collateral = tok_sub tok Constants.creation_deposit;
      }
  in

  burrow_out

(** Deativate a currently active burrow. This operation will fail if the burrow
  * (a) is already inactive, or (b) is overburrowed, or (c) has kit
  * outstanding, or (d) has collateral sent off to auctions. *)
let burrow_deactivate (p: Parameters.t) (b: burrow) : (burrow * tok) =
  let b = burrow_touch p b in
  let burrow_out, return =
    if burrow_is_overburrowed p b then
      (failwith error_DeactivatingAnOverburrowedBurrow : (burrow * tok))
    else if (not b.active) then
      (failwith error_DeactivatingAnInactiveBurrow : (burrow * tok))
    else if gt_kit_kit b.outstanding_kit kit_zero then
      (failwith error_DeactivatingWithOutstandingKit : (burrow * tok))
    else if gt_tok_tok b.collateral_at_auction tok_zero then
      (failwith error_DeactivatingWithCollateralAtAuctions : (burrow * tok))
    else
      let return = tok_add b.collateral Constants.creation_deposit in
      let updated_burrow =
        { b with
          active = false;
          collateral = tok_zero;
        } in
      (updated_burrow, return)
  in

  burrow_out, return

(* ************************************************************************* *)
(**                          LIQUIDATION-RELATED                             *)
(* ************************************************************************* *)

(** Compute the number of tez that needs to be auctioned off so that the burrow
  * can return to a state when it is no longer overburrowed or having a risk of
  * liquidation (assuming the current expected minting price). For its
  * calculation, see docs/burrow-state-liquidations.md.  Note that it's skewed
  * on the safe side (overapproximation). This ensures that after a partial
  * liquidation we are no longer "optimistically overburrowed".
  * Returns the number of tez in mutez *)
let compute_collateral_to_auction (p: Parameters.t) (b: burrow) : int =

  let { num = num_fm; den = den_fm; } = Constants.fminting in
  let { num = num_mp; den = den_mp; } = Parameters.minting_price p in
  (* Note that num_lp and den_lp here are actually = 1 - liquidation_penalty *)
  let { num = num_lp; den = den_lp; } =
    let { num = num_lp; den = den_lp; } = Constants.liquidation_penalty in
    { num = den_lp - num_lp; den = den_lp; }
  in

  (* numerator = tez_sf * den_lp * num_fm * num_mp * outstanding_kit
     - kit_sf * den_mp * (num_lp * num_fm * collateral_at_auctions + den_lp * den_fm * collateral) *)
  let numerator =
      (
         tok_scaling_factor_int
         *
         (
            den_lp
            *
            (
               num_fm
               *
               (
                  num_mp
                  *
                  (kit_to_denomination_nat b.outstanding_kit)
               )
            )
         )
      )
      -
      (
         (kit_scaling_factor_int * den_mp)
         *
         (
            (num_lp * (num_fm * (Tok.tok_to_denomination_nat b.collateral_at_auction)))
            +
            (den_lp * (den_fm * (Tok.tok_to_denomination_nat b.collateral)))
         )
      ) in
  (* denominator = (kit_sf * den_mp * tez_sf) * (num_lp * num_fm - den_lp * den_fm) *)
  let denominator =
      kit_scaling_factor_int
      *
      (
         den_mp
         *
         (
            Tok.tok_scaling_factor_int
            *
            (
               (num_lp * num_fm)
               -
               (den_lp * den_fm)
            )
         )
      ) in
  Common.cdiv_int_int (numerator * Tok.tok_scaling_factor_int) denominator

(** Compute the amount of kit we expect to receive from auctioning off an
  * amount of tez, using the current minting price. Since this is an artifice,
  * a mere expectation, we neither floor nor ceil, but instead return the
  * lossless fraction as is. *)
let compute_expected_kit (p: Parameters.t) (collateral_to_auction: tok) : Common.ratio =
  let { num = num_lp; den = den_lp; } = Constants.liquidation_penalty in
  let { num = num_mp; den = den_mp; } = Parameters.minting_price p in
  let numerator =
      (Tok.tok_to_denomination_nat collateral_to_auction)
      *
      (
         (den_lp - num_lp)
         *
         den_mp
      ) in
  let denominator =
      Tok.tok_scaling_factor_int
      *
      (den_lp * num_mp) in
  { num = numerator; den = denominator; }

(** Check whether a burrow can be marked for liquidation. A burrow can be
  * marked for liquidation if:
  *
  *   tez_collateral < fliquidation * (kit_outstanding - expected_kit_from_auctions) * liquidation_price
  *
  * The quantity tez_collateral / (fliquidation * liquidation_price) we call the
  * liquidation limit. Note that for this check we optimistically take into
  * account the expected kit from pending auctions (using the current minting
  * price) when computing the outstanding kit. Note that only active burrows
  * can be liquidated; inactive ones are dormant, until either all pending
  * auctions finish or if their creation deposit is restored. *)
let burrow_is_liquidatable (p: Parameters.t) (b: burrow) : bool =


  let tez = { num = tok_to_denomination_int b.collateral; den = tok_scaling_factor_int; } in
  let kit = (* kit = kit_outstanding - expected_kit_from_auctions *)
    let { num = num_ek; den = den_ek; } = compute_expected_kit p b.collateral_at_auction in
    { num =
          ((Kit.kit_to_denomination_nat b.outstanding_kit) * den_ek)
          -
          (Kit.kit_scaling_factor_int * num_ek);
      den = kit_scaling_factor_int * den_ek;
    } in
  b.active && undercollateralization_condition Constants.fliquidation (Parameters.liquidation_price p) tez kit

(** Check whether the return of a slice to its burrow (cancellation) is
  * warranted. For the cancellation to be warranted, it must be the case that
  * after returning the slice to the burrow, the burrow is optimistically
  * non-overburrowed (i.e., if all remaining collateral at auction sells at the
  * current price but with penalties paid, the burrow becomes underburrowed):
  *
  *   collateral + slice >= fminting * (outstanding - compute_expected_kit (collateral_at_auction - slice)) * minting_price
  *
  * Note that only active burrows can be liquidated; inactive ones are dormant,
  * until either all pending auctions finish or if their creation deposit is
  * restored. *)
let burrow_is_cancellation_warranted (p: Parameters.t) (b: burrow) (slice_tok: tok) : bool =



  let tez = (* tez = collateral + slice *)
    { num = tok_to_denomination_int (tok_add b.collateral slice_tok);
      den = tok_scaling_factor_int;
    } in
  let kit = (* kit = outstanding - compute_expected_kit (collateral_at_auction - slice) *)
    let { num = num_ek; den = den_ek; } =
      compute_expected_kit p (tok_sub b.collateral_at_auction slice_tok) in
    { num =
          ((Kit.kit_to_denomination_nat b.outstanding_kit) * den_ek)
          -
          (Kit.kit_scaling_factor_int * num_ek);
      den = kit_scaling_factor_int * den_ek;
    } in

  b.active && not (undercollateralization_condition Constants.fminting (Parameters.minting_price p) tez kit)

(** Compute the minumum amount of kit to receive for considering the
  * liquidation unwarranted, calculated as (see
  * docs/burrow-state-liquidations.md for the derivation of this formula):
  *
  *   collateral_to_auction * (fliquidation * (outstanding_kit - expected_kit_from_auctions)) / collateral
  *
  * If the burrow has no collateral left in it (e.g., right after a successful
  * Complete-liquidation) then we have two cases:
  * (a) If the outstanding kit is non-zero then there is no way for this
  *     liquidation to be considered unwarranted. outstanding_kit is infinitely
  *     many times greater than the collateral.
  * (b) If the outstanding kit is also zero then the liquidation in question
  *     shouldn't have happened (so it is by definition unwarranted). I think
  *     that this is impossible in practice, but it's probably best to account
  *     for it so that the function is not partial.
*)
[@inline] let compute_min_kit_for_unwarranted (p: Parameters.t) (b: burrow) (collateral_to_auction: tok) : kit option =


  if b.collateral = tok_zero (* NOTE: division by zero. *)
  then
    if not (eq_kit_kit b.outstanding_kit (kit_of_denomination (0n)))
    then (None: kit option) (* (a): infinity, basically *)
    else (Some kit_zero) (* (b): zero *)
  else
    let { num = num_fl; den = den_fl; } = Constants.fliquidation in
    let { num = num_ek; den = den_ek; } = compute_expected_kit p b.collateral_at_auction in

    (* numerator = max 0 (collateral_to_auction * num_fl * (den_ek * outstanding_kit - kit_sf * num_ek)) *)
    let numerator =
      let numerator =
          ((tok_to_denomination_nat collateral_to_auction) * num_fl)
          *
          (
             (den_ek * (kit_to_denomination_nat b.outstanding_kit))
             -
             (kit_scaling_factor_int * num_ek)
          ) in
      Common.max_int 0 numerator in

    (* denominator = collateral * den_fl * kit_sf * den_ek *)
    let denominator =
        ((tok_to_denomination_nat b.collateral) * den_fl)
        *
        (kit_scaling_factor_int * den_ek) in

    Some (kit_of_fraction_ceil numerator denominator) (* Round up here; safer for the system, less so for the burrow *)

let burrow_request_liquidation (p: Parameters.t) (b: burrow) : liquidation_result =
  let b = burrow_touch p b in
  let partial_reward =
    let { num = num_lrp; den = den_lrp; } = Constants.liquidation_reward_percentage in
    tok_of_fraction_floor
      ((tok_to_denomination_nat b.collateral) * num_lrp)
      (tok_scaling_factor_int * den_lrp)
  in
  if not (burrow_is_liquidatable p b) then
    (* Case 1: The outstanding kit does not exceed the liquidation limit, or
     * the burrow is already without its creation deposit, inactive; we
     * shouldn't liquidate the burrow. *)
    (None : liquidation_result)
  else
    let liquidation_reward = tok_add Constants.creation_deposit partial_reward in
    if lt_tok_tok (tok_sub b.collateral partial_reward) Constants.creation_deposit then
      (* Case 2a: Cannot even refill the creation deposit; liquidate the whole
       * thing (after paying the liquidation reward of course). *)
      let collateral_to_auction = tok_sub b.collateral partial_reward in
      let final_burrow =
        { b with
          active = false;
          collateral = tok_zero;
          collateral_at_auction = tok_add b.collateral_at_auction collateral_to_auction;
        } in
      Some
        ( Close,
          { liquidation_reward = liquidation_reward;
            collateral_to_auction = collateral_to_auction;
            burrow_state = final_burrow; }
        )
    else
      (* Case 2b: We can replenish the creation deposit. Now we gotta see if it's
       * possible to liquidate the burrow partially or if we have to do so
       * completely (deplete the collateral). *)
      let b_without_reward = { b with collateral = tok_sub (tok_sub b.collateral partial_reward) Constants.creation_deposit } in
      let collateral_to_auction = compute_collateral_to_auction p b_without_reward in

      (* FIXME: The property checked by the following assertion is quite
       * intricate to prove. We probably should include the proof somewhere
       * in the codebase. *)


      if collateral_to_auction > (tok_to_denomination_int b_without_reward.collateral) then
        (* Case 2b.1: With the current price it's impossible to make the burrow
         * not undercollateralized; pay the liquidation reward, stash away the
         * creation deposit, and liquidate all the remaining collateral, even if
         * it is not expected to repay enough kit. *)
        let collateral_to_auction = b_without_reward.collateral in (* OVERRIDE *)
        let final_burrow =
          { b with
            collateral = tok_zero;
            collateral_at_auction = tok_add b.collateral_at_auction collateral_to_auction;
          } in
        Some
          ( Complete,
            { liquidation_reward = liquidation_reward;
              collateral_to_auction = collateral_to_auction;
              burrow_state = final_burrow; }
          )
      else
        (* Case 2b.2: Recovery is possible; pay the liquidation reward, stash away the
         * creation deposit, and liquidate the collateral needed to underburrow
         * the burrow (assuming that the past auctions will be successful but
         * warranted, and that the liquidation we are performing will also be
         * deemed warranted). If---when the auction is over---we realize that the
         * liquidation was not really warranted, we shall return the auction
         * earnings in their entirety. If not, then only 90% of the earnings
         * shall be returned. *)
        let collateral_to_auction = match is_nat collateral_to_auction with
          | Some collateral -> tok_of_denomination collateral
          (* Note: disabling coverage for this line since it really should be impossible to reach this line *)
          | None -> (failwith internalError_ComputeTezToAuctionNegativeResult : tok)
                    (* [@coverage off] *)
        in
        let final_burrow =
          { b with
            collateral = tok_sub b_without_reward.collateral collateral_to_auction;
            collateral_at_auction = tok_add b.collateral_at_auction collateral_to_auction;
          } in
        Some
          ( Partial,
            { liquidation_reward = liquidation_reward;
              collateral_to_auction = collateral_to_auction;
              burrow_state = final_burrow; }
          )
