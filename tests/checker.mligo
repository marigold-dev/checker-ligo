#import "../breathalyzer/lib/lib.mligo" "Breath"
#import "../src/checker.mligo" "Checker"
#import "../src/common.mligo" "Common"
#import "../src/cfmm.mligo" "CFMM"
#import "../src/fixedPoint.mligo" "FixedPoint"
#import "../src/kit.mligo" "Kit"
#import "../src/lqt.mligo" "Lqt"
#import "./ratio.mligo" "Ratio"
#import "../src/ctok.mligo" "Ctok"
#import "../src/wtez.mligo" "Wtez"
#import "../src/wctez.mligo" "WCtez"
#import "../src/checkerTypes.mligo" "CheckerT"
#import "../utils/mock_cfmm_oracle.mligo" "MockCFMM"
#import "../utils/mock_oracle.mligo" "MockOracle"
#import "../src/checkerMain.mligo" "CheckerMain"
#import "../src/error.mligo" "Errors"


type level = Breath.Logger.level
type originated = Breath.Contract.originated

module Ctez = struct

#import "../vendor/ctez/ctez.mligo" "Ctez"
#import "../vendor/ctez/cfmm_tez_ctez.mligo" "CtezCFMM"
#import "../vendor/ctez/fa12.mligo" "FA12"

  type ctez_contracts = {
    ctez: address;
    fa12_ctez: address;
    cfmm: address;
  }

  let deploy_ctez (level: level) =
    let ctez_storage : Ctez.storage =
      {
        ovens = Big_map.empty;
        target = Bitwise.shift_left 1n 48n;
        drift = 0;
        last_drift_update = Tezos.get_now ();
        ctez_fa12_address = ("tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU" : address);
        cfmm_address = ("tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU" : address);
      }
    in
    let ctez =
      Breath.Contract.originate
        level
        "ctez"
        Ctez.main
        ctez_storage
        0tez
    in
    let fa12_ctez_storage : FA12.storage =
      let ledger = Big_map.empty in
      let ledger = Big_map.add ("tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU" : address) 1n ledger in
      {
        tokens = ledger;
        allowances = (Big_map.empty : (FA12.allowance_key, nat) big_map);
        admin = ctez.originated_address;
        total_supply = 1n
      }
    in
    let fa12_ctez =
      Breath.Contract.originate
        level
        "ctez_fa12"
        FA12.main
        fa12_ctez_storage
        0tez
    in
    let ctez_cfmm_storage : CtezCFMM.storage = {
      tezPool = 1n;
      cashPool = 1n;
      target = Bitwise.shift_left 1n 48n;
      lqtTotal = 1n;
      ctez_address = ctez.originated_address;
      cashAddress = fa12_ctez.originated_address;
      lqtAddress = ("tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU" : address);
      lastOracleUpdate = Tezos.get_now ();
      consumerEntrypoint = ctez.originated_address
    }
    in
    let ctez_cfmm =
      Breath.Contract.originate
        level
        "ctez_cfmm"
        CtezCFMM.main
        ctez_cfmm_storage
        0tez
    in
    {
      ctez = ctez.originated_address;
      fa12_ctez = fa12_ctez.originated_address;
      cfmm = ctez_cfmm.originated_address
    }
end

let ctez_contracts (level: level) : Ctez.ctez_contracts =
  Ctez.deploy_ctez level

let oracle_contract (owner: address) (level: level) =
  Breath.Contract.originate
    level
    "oracle"
    MockOracle.main
    (MockOracle.make_storage owner)
    0tez

let wtez_contract (level: level) =
  Breath.Contract.originate
    level
    "wtez"
    Wtez.main
    (Wtez.initial_wtez ())
    0tez

let wctez_contract (level: level) (ctez_fa12_address: address) =
  Breath.Contract.originate
    level
    "wctez"
    WCtez.main
    (WCtez.initial_wctez ctez_fa12_address)
    0tez

let originate_cfmm (owner: address) (level: level) =
  Breath.Contract.originate
    level
    "cfmm"
    MockCFMM.main
    (MockCFMM.make_storage owner)
    0tez

(* TODO: tests with other FA2 collaterals *)

let originate_external_contracts (level: level) (alice: Breath.Context.actor) =
  let ctez_contracts = ctez_contracts level in
  let oracle = oracle_contract alice.address level in
  let collateral_fa2 = wtez_contract level in
  let ctok_fa2 = wctez_contract level ctez_contracts.fa12_ctez in
  {
    oracle = oracle.originated_address;
    collateral_fa2 = collateral_fa2.originated_address;
    ctok_fa2 = ctok_fa2.originated_address;
    ctez_cfmm = ctez_contracts.cfmm
  }
  (*
  in
  CheckerT.initial_checker external_contracts
  *)

let originate_checker (level : level) (alice: Breath.Context.actor) =
    Breath.Contract.originate
    level
    "checker"
    CheckerMain.main
    (CheckerMain.initial_wrapper alice.address)
    0tez

let (_, (alice, _bob, _carol)) = Breath.Context.init_default ()

let test_checker_origination =
  Breath.Model.case
    "checker_can_be_originated"
    ""
    (fun level ->
      let _checker = originate_checker level in
      Breath.Assert.is_true "" true)

let act (contract : (CheckerMain.params, CheckerT.wrapper) originated) (qty: tez) act () =
  Breath.Contract.transfer_to contract act qty

let is_sealed (checker_state: CheckerT.wrapper) = match checker_state.deployment_state with
  | Unsealed _ -> false
  | Sealed _ -> true
(* TODO
  Breath.Assert.is_true "storage_is_sealed" (is_sealed storage) *)

let test_checker_sealing =
  Breath.Model.case
    "checker_sealing_fails_if_incomplete"
    ""
    (fun level ->
      let checker = originate_checker level alice in
      let initial_checker = originate_external_contracts level alice in
      let sealing = SealContract initial_checker in
      let alice_sealing = Breath.Context.act_as alice (act checker 0tez sealing) in
      let storage = Breath.Contract.storage_of checker in
      Breath.Result.reduce [
        Breath.Expect.fail_with_message Errors.error_GetLazyFunctionMissingFunction (alice_sealing)
      ])


let suite = [
  test_checker_origination;
  test_checker_sealing
]
