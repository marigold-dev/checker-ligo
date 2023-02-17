#import "../breathalyzer/lib/lib.mligo" "Breath"
#import "../src/parameters.mligo" "Parameters"
#import "../src/burrow.mligo" "Burrow"
#import "../src/kit.mligo" "Kit"
#import "../src/tok.mligo" "Tok"
#import "../src/fixedPoint.mligo" "Fixedpoint"

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

  let with_outstanding (burrow: Burrow.burrow) n =
    { burrow with outstanding_kit = n }

  let suite = [
    Breath.Model.case
    "burrow_burn_kit"
    "does not fail for a burrow which needs to be touched"
    (fun (_: level) ->
      let last_touched = Tezos.get_now() + 1 in
      let initial_parameters = Parameters.initial_parameters in
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
        let burrow, actual_burned = Burrow.burrow_burn_kit Parameters.initial_parameters kit_to_burn
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
          Parameters.initial_parameters
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
        let burrow, actual_burned = Burrow.burrow_burn_kit Parameters.initial_parameters kit_to_burn
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
        let burrow, actual_burned = Burrow.burrow_burn_kit Parameters.initial_parameters kit_to_burn
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
        let new_burrow, _ = Burrow.burrow_burn_kit Parameters.initial_parameters kit_to_burn
        burrow in
        Breath.Result.reduce [
          Breath.Assert.is_equal "burrow address" new_burrow.address burrow.address;
        ]);
  ]

end

let () =
  Breath.Model.run_suites Void [
    Breath.Model.suite "Suite for Batcher"
      Burrow.suite
  ]

