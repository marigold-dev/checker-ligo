#import "../breathalyzer/lib/lib.mligo" "Breath"
#import "../src/common.mligo" "Common"
#import "../src/parameters.mligo" "Parameters"
#import "../src/burrow.mligo" "Burrow"
#import "../src/kit.mligo" "Kit"
#import "../src/tok.mligo" "Tok"
#import "../src/fixedPoint.mligo" "Fixedpoint"
#import "../math-lib-cameligo/core/math.mligo" "Math"
#import "../src/tokenMetadata.mligo" "Metadata"
#import "./ratio.mligo" "Ratio"
#import "./kit.mligo" "KitTest"
#import "./cfmm.mligo" "CFMMTest"

type level = Breath.Logger.level

module Burrow = struct

  let make_burrow
    active
    address
    collateral
    outstanding_kit
    adjustment_index
    collateral_at_auction
    last_checker_timestamp : Burrow.burrow =
  {
    address = address;
    active = active;
    collateral = collateral;
    outstanding_kit = outstanding_kit;
    adjustment_index = adjustment_index;
    collateral_at_auction = collateral_at_auction;
    last_checker_timestamp = last_checker_timestamp;
  }

  let burrow =
    make_burrow
      true
      ("KT1JVJLCqFBvcN8RwBAfQy56UZVw5E4THdjo" : address)
      Tok.tok_zero
      (Kit.kit_of_denomination 0n)
      Fixedpoint.fixedpoint_one
      Tok.tok_zero
      (Tezos.get_now ())

  let initial_parameters = Parameters.initial_parameters

  let with_outstanding (burrow: Burrow.burrow) n =
    { burrow with outstanding_kit = n }

  let suite = [
    Breath.Model.case
      "burrow_burn_kit"
      "does not fail for a burrow which needs to be touched"
      (fun (_: level) ->
        let last_touched = Tezos.get_now() + 1 in
        let parameters = { initial_parameters with last_touched=last_touched } in
        let _burrow, _actual_kit =
          Burrow.burrow_burn_kit
            parameters
            (Kit.kit_of_denomination 1n)
            burrow
        in
        Breath.Assert.succeeds);

    Breath.Model.case
      "burrow_burn_kit"
      "burning 0 kit returns burrow with same outstanding kit"
      (fun _ ->
        let burrow = with_outstanding burrow 3n in
        let kit_to_burn = Kit.kit_of_denomination 0n in
        let burrow, actual_burned = Burrow.burrow_burn_kit initial_parameters kit_to_burn
        burrow in
        Breath.Result.reduce [
          Breath.Assert.is_equal "outstanding amount of kit" burrow.outstanding_kit 3n;
          Breath.Assert.is_equal "burned amount of kit" actual_burned 0n
        ]);

    Breath.Model.case
      "burrow_burn_kit"
      "burning exactly outstanding_kit returns burrow with expected outstanding kit"
      (fun _ ->
        let burrow = with_outstanding burrow 1n in
        let kit_to_burn = Kit.kit_of_denomination 1n in
        let burrow, actual_burned =
          Burrow.burrow_burn_kit
            initial_parameters
            kit_to_burn
            burrow
        in
        Breath.Result.and_then
          (Breath.Assert.is_equal "outstanding amount of kit" burrow.outstanding_kit Kit.kit_zero)
          (Breath.Assert.is_equal "burned amount of kit" actual_burned kit_to_burn));

    Breath.Model.case
      "burrow_burn_kit"
      "burning more than outstanding_kit returns burrow with expected outstanding kit"
      (fun _ ->
        let burrow = with_outstanding burrow 1n in
        let kit_to_burn = Kit.kit_of_denomination 2n in
        let burrow, actual_burned = Burrow.burrow_burn_kit initial_parameters kit_to_burn
        burrow in
        Breath.Result.reduce [
          Breath.Assert.is_equal "outstanding amount of kit" burrow.outstanding_kit Kit.kit_zero;
          Breath.Assert.is_equal "burned amount of kit" actual_burned 1n
        ]);

    Breath.Model.case
      "burrow_burn_kit"
      "burning less than outstanding_kit returns burrow with expected outstanding kit"
      (fun _ ->
        let burrow = with_outstanding burrow 3n in
        let kit_to_burn = Kit.kit_of_denomination 1n in
        let burrow, actual_burned = Burrow.burrow_burn_kit initial_parameters kit_to_burn
        burrow in
        Breath.Result.reduce [
          Breath.Assert.is_equal "outstanding amount of kit" burrow.outstanding_kit 2n;
          Breath.Assert.is_equal "burned amount of kit" actual_burned kit_to_burn
        ]);

    Breath.Model.case
      "burrow_burn_kit"
      "burning kits does not change the burrow address"
      (fun _ ->
        let burrow = with_outstanding burrow 3n in
        let kit_to_burn = Kit.kit_of_denomination 1n in
        let new_burrow, _ = Burrow.burrow_burn_kit initial_parameters kit_to_burn
        burrow in
        Breath.Assert.is_equal "burrow address" new_burrow.address burrow.address);

    (*** burrow_withdraw_collateral ***)

    Breath.Model.case
      "burrow_withdraw_collateral"
      "does not fail for a burrow that needs to be touched"
      (fun _ ->
        let last_touched = Tezos.get_now() + 1 in
        let parameters = { initial_parameters with last_touched=last_touched } in
        let _burrow =
          Burrow.burrow_withdraw_collateral
            parameters
            Tok.tok_zero
            burrow
        in
        Breath.Assert.succeeds);

    Breath.Model.case
      "burrow_withdraw_collateral"
      "after succesful withdrawal, burrow has expected collateral"
      (fun _ ->
        let burrow = with_outstanding burrow 1n in
        let burrow = { burrow with collateral = 100n } in
        let burrow =
          Burrow.burrow_withdraw_collateral
            initial_parameters
            (Tok.tok_of_denomination 1n)
            burrow
        in
        Breath.Assert.is_equal "burrow collateral" burrow.collateral (Tok.tok_of_denomination 99n));

    Breath.Model.case
      "burrow_withdraw_collateral"
      "does not change burrow address"
      (fun _ ->
        let burrow = with_outstanding burrow 1n in
        let burrow = { burrow with collateral = 100n } in
        let new_burrow =
          Burrow.burrow_withdraw_collateral
            initial_parameters
            (Tok.tok_of_denomination 1n)
            burrow
        in
        Breath.Assert.is_equal "burrow address" new_burrow.address burrow.address);

(* FIXME: can't do this for now

    Breath.Model.case
      "burrow_withdraw_collateral"
      "fails if the burrow does not have enough collateral"
      (fun _ ->
        Breath.Result.try_catch
        (fun () ->
          let burrow = with_outstanding burrow 4n in
          let burrow = { burrow with collateral = 10n } in
          let _ =
            Burrow.burrow_withdraw_collateral
              initial_parameters
              (Tok.tok_of_denomination 2n)
              burrow
          in
          (Success 0n : test_exec_result))
        (fun _ -> Failed [Message "should have failed"])
        (fun _ -> Passed 0n));
*)

    Breath.Model.case
      "burrow_mint_kit"
      "burrow after successful minting has expected outstanding"
      (fun _ ->
        let burrow = { burrow with outstanding_kit = 1n; collateral = 100n } in
        let burrow =
          Burrow.burrow_mint_kit
            initial_parameters
            (Tok.tok_of_denomination 1n)
            burrow
        in
        Breath.Assert.is_equal "" burrow.outstanding_kit 2n);

    Breath.Model.case
      "burrow_mint_kit"
      "does not fail for a burrow which needs to be touched"
      (fun _ ->
        let last_touched = Tezos.get_now () - 1 in
        let parameters = { initial_parameters with last_touched = last_touched } in
        let _burrow =
          Burrow.burrow_mint_kit
          parameters
          (Kit.kit_of_denomination 0n)
          burrow
        in
        Breath.Assert.succeeds);

  (* TODO: make sure burrow_max_mintable_kit is correct, relatively to parameters *)

    Breath.Model.case
      "burrow_mint_kit"
      "minting burrow_max_mintable_kit succeeds"
      (fun _ ->
        let burrow = { burrow with outstanding_kit = 0n; collateral = Tok.tok_of_denomination
          12345678904n } in
        let burrow_max_mintable = Burrow.burrow_max_mintable_kit initial_parameters burrow in
        let burrow = Burrow.burrow_mint_kit initial_parameters burrow_max_mintable burrow in
        Breath.Assert.is_equal "" burrow.outstanding_kit burrow_max_mintable);

  (* FIXME: can't do this for the moment
    Breath.Model.case
      "burrow_mint_kit"
      "minting more than burrow_max_mintable_kit fails"
      (fun _ ->
        let burrow = { burrow with outstanding_kit = 0n; collateral = Tok.tok_of_denomination
          12345678904n } in
        let burrow_max_mintable = Burrow.burrow_max_mintable_kit initial_parameters burrow in
        let just_over_max = burrow_max_mintable + 1n in
        let burrow = Burrow.burrow_mint_kit initial_parameters just_over_max burrow in
        Breath.Assert.is_equal "" burrow.outstanding_kit burrow_max_mintable);
    *)

  ]

end

let () =
  Breath.Model.run_suites Void [
    Breath.Model.suite "Tests for burrows" Burrow.suite;
    Breath.Model.suite "Tests for kit" KitTest.suite;
    Breath.Model.suite "Tests for CFMM" CFMMTest.suite
  ]

