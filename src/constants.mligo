(* Automatically generated by checker_tools.builder. DO NOT EDIT DIRECTLY! *)

#import "./common.mligo" "Common"
#import "./tok.mligo" "Tok"

[@inline] let fminting : Common.ratio = Common.make_ratio ((21)) ((10))

[@inline] let fliquidation : Common.ratio = Common.make_ratio ((19)) ((10))

[@inline] let creation_deposit : Tok.t = Tok.tok_of_denomination (1000000n)

[@inline] let burrow_fee_percentage : Common.ratio = Common.make_ratio ((5)) ((1000))

[@inline] let imbalance_scaling_factor : Common.ratio = Common.make_ratio ((3)) ((4))

[@inline] let imbalance_limit = Common.make_ratio ((5)) ((100))

[@inline] let liquidation_reward_percentage : Common.ratio = Common.make_ratio ((1)) ((1000))

[@inline] let cfmm_fee : Common.ratio = Common.make_ratio ((2)) ((1000))

[@inline] let protected_index_inverse_epsilon : int = (120000)

[@inline] let max_lot_size : Tok.t = Tok.tok_of_denomination (10000000000n)

[@inline] let min_lot_auction_queue_fraction : Common.ratio = Common.make_ratio ((5)) ((100))

[@inline] let liquidation_penalty : Common.ratio = Common.make_ratio ((1)) ((10))

[@inline] let seconds_in_a_year : int = (31556952)

[@inline] let seconds_in_a_day : int = (86400)

[@inline] let auction_decay_rate : Common.ratio = Common.make_ratio ((1)) ((6000))

[@inline] let max_bid_interval_in_seconds : int = (1200)

[@inline] let max_bid_interval_in_blocks : nat = 20n

[@inline] let bid_improvement_factor : Common.ratio = Common.make_ratio ((33)) ((10000))

[@inline] let touch_reward_low_bracket : int = (600)

[@inline] let touch_low_reward : Common.ratio = Common.make_ratio ((1)) ((600))

[@inline] let touch_high_reward : Common.ratio = Common.make_ratio ((1)) ((60))

[@inline] let number_of_slices_to_process : nat = 5n

[@inline] let max_liquidation_queue_height: nat = 12n
