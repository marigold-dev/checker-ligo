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
#import "../src/checkerEntrypoints.mligo" "Entrypoints"
#import "../src/error.mligo" "Errors"

let address (type a b) (x: (a, b) typed_address): address =
  let c = Test.to_contract x in
  Tezos.address c

module Ctez = struct

#import "../vendor/ctez/ctez.mligo" "Ctez"
#import "../vendor/ctez/cfmm_tez_ctez.mligo" "CtezCFMM"
#import "../vendor/ctez/fa12.mligo" "FA12"

  type ctez_contracts = {
    ctez: address;
    fa12_ctez: address;
    cfmm: address;
  }

  let deploy_ctez (_: unit) =
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
    let {addr = ctez_address; code = _; size = _} =
      Test.originate (contract_of Ctez) ctez_storage 0tez
    in
    let fa12_ctez_storage : FA12.storage =
      let ledger = Big_map.empty in
      let ledger = Big_map.add ("tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU" : address) 1n ledger in
      {
        tokens = ledger;
        allowances = (Big_map.empty : (FA12.allowance_key, nat) big_map);
        admin = address ctez_address;
        total_supply = 1n
      }
    in
    let {addr = fa12_ctez_address; code = _; size = _} =
      Test.originate (contract_of FA12) fa12_ctez_storage 0tez
    in
    let ctez_cfmm_storage : CtezCFMM.storage = {
      tezPool = 1n;
      cashPool = 1n;
      target = Bitwise.shift_left 1n 48n;
      lqtTotal = 1n;
      ctez_address = address ctez_address;
      cashAddress = address fa12_ctez_address;
      lqtAddress = ("tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU" : address);
      lastOracleUpdate = Tezos.get_now ();
      consumerEntrypoint = address ctez_address
    }
    in
    let {addr = ctez_cfmm_address; code = _; size = _} =
      Test.originate (contract_of CtezCFMM) ctez_cfmm_storage 0tez
    in
    {
      ctez = address ctez_address;
      fa12_ctez = address fa12_ctez_address;
      cfmm = address ctez_cfmm_address
    }
end

let ctez_contracts (_: unit) : Ctez.ctez_contracts =
  Ctez.deploy_ctez ()

let oracle_contract (owner: address) =
  (Test.originate (contract_of MockOracle) (MockOracle.make_storage owner) 0tez).addr

let wtez_contract (_: unit) =
  (Test.originate (contract_of Wtez) (Wtez.initial_wtez ()) 0tez).addr

let wctez_contract (ctez_fa12_address: address) =
  (Test.originate (contract_of WCtez) (WCtez.initial_wctez ctez_fa12_address) 0tez).addr

let originate_cfmm (owner: address) =
  (Test.originate (contract_of MockCFMM) (MockCFMM.make_storage owner) 0tez).addr

(* TODO: tests with other FA2 collaterals *)

let originate_external_contracts (alice: address) =
  let ctez_contracts = ctez_contracts () in
  let oracle_address = oracle_contract alice in
  let collateral_fa2_address = wtez_contract () in
  let ctok_fa2_address = wctez_contract ctez_contracts.fa12_ctez in
  {
    oracle = oracle_address;
    collateral_fa2 = address collateral_fa2_address;
    ctok_fa2 = ctok_fa2_address;
    ctez_cfmm = ctez_contracts.cfmm
  }

let originate_checker (alice: address) =
  (Test.originate (contract_of CheckerMain) (CheckerMain.initial_wrapper alice) 0tez).addr

(* FIXME Can't use this either because we can't serialize entrypoints correctly
let deploy_entrypoint (fun_id: int) (lazy_fun: (CheckerT.checker * bytes) -> operation list * CheckerT.checker) checker =
  let fun_bytes = (Bytes.pack lazy_fun: bytes) in
  (* We arbitrarily split bytes to test concatenation of the entrypoints *)
  let n = Bytes.length fun_bytes in
  let fun_bytes1 = Bytes.sub 0n (abs (n/2)) fun_bytes in
  let fun_bytes2 = Bytes.sub (abs (n/2)) (abs ((n+1)/2)) fun_bytes in
  let deploy1 = DeployFunction (fun_id, fun_bytes1) in
  let deploy2 = DeployFunction (fun_id, fun_bytes2) in
  let act1 = act checker 0tez deploy1 in
  let act2 = act checker 0tez deploy2 in
  (act1, act2)
*)

let suite () =
  let alice = Test.nth_bootstrap_account 1 in
  let checker_address = originate_checker alice in
  (address checker_address)

let lazy_entrypoints = [
  (Entrypoints.lazy_fun_create_burrow , (1));
  (Entrypoints.lazy_fun_deposit_collateral , (2));
  (Entrypoints.lazy_fun_withdraw_collateral , (3));
  (Entrypoints.lazy_fun_mint_kit , (4));
  (Entrypoints.lazy_fun_burn_kit , (5));
  (Entrypoints.lazy_fun_activate_burrow , (6));
  (Entrypoints.lazy_fun_deactivate_burrow , (7));
  (Entrypoints.lazy_fun_mark_for_liquidation , (8));
  (Entrypoints.lazy_fun_touch_liquidation_slices , (9));
  (Entrypoints.lazy_fun_cancel_liquidation_slice , (10));
  (Entrypoints.lazy_fun_touch_burrow , (11));
  (Entrypoints.lazy_fun_set_burrow_delegate , (12));
  (Entrypoints.lazy_fun_buy_kit , (13));
  (Entrypoints.lazy_fun_sell_kit , (14));
  (Entrypoints.lazy_fun_add_liquidity , (15));
  (Entrypoints.lazy_fun_remove_liquidity , (16));
  (Entrypoints.lazy_fun_liquidation_auction_place_bid , (17));
  (Entrypoints.lazy_fun_liquidation_auction_claim_win , (18));
  (Entrypoints.lazy_fun_receive_price , (19));
  (Entrypoints.lazy_fun_receive_ctez_marginal_price , (20));
  (Entrypoints.lazy_fun_update_operators , (21));
]

(*
let test1 =
  let checker_address = suite () in
  (* let _packed_entrypoints = List.map Bytes.pack lazy_entrypoints in *)
  let _ = List.map
    (fun (ent, nth) ->
      let arg = DeployFunction (nth, Bytes.pack ent) in
      ())
    lazy_entrypoints
  in
  ()
*)

let _ =
  let x = Bytes.pack (Entrypoints.lazy_fun_sell_kit) in
  x
