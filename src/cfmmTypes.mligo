#import "../src/common.mligo" "Common"
#import "../src/kit.mligo" "Kit"
#import "../src/ctok.mligo" "Ctok"
#import "../src/lqt.mligo" "Lqt"

type cfmm =
  { ctok: Ctok.ctok;
    kit: Kit.kit;
    lqt: Lqt.lqt;
    kit_in_ctok_in_prev_block: Common.ratio (* [@printer pp_ratio] *);
    last_level: nat;
  }

(** The initial state of the cfmm contract. We always start with the lowest
    denomination of kit, ctok, and liquidity tokens (effectively setting the
    starting price to 1 ctok/kit). The price will eventually reach the value it
    should, but this saves us from having the first/non-first liquidity
    provider separation, and all division-by-zero checks. *)
let initial_cfmm () : cfmm =
  { ctok = Ctok.ctok_of_denomination (1n);
    kit = Kit.kit_of_denomination (1n);
    lqt = Lqt.lqt_of_denomination (1n);
    kit_in_ctok_in_prev_block = Common.one_ratio; (* Same as ctok/kit now. *)
    last_level = Tezos.get_level ();
  }
