#import "./fa2Interface.mligo" "FA2"

(* issued *)

(* kit *)
[@inline] let kit_token_id : FA2.fa2_token_id = 0n
[@inline] let kit_decimal_digits : nat = 6n

(* lqt *)
[@inline] let lqt_token_id : FA2.fa2_token_id = 1n
[@inline] let lqt_decimal_digits : nat = 6n

(* wtez *)
[@inline] let wtez_token_id : FA2.fa2_token_id = 2n
[@inline] let wtez_decimal_digits : nat = 6n

(* wctez *)
[@inline] let wctez_token_id : FA2.fa2_token_id = 3n
[@inline] let wctez_token_decimal_digits : nat = 6n

(* mock_fa2 *)
[@inline] let mock_fa2_token_id : FA2.fa2_token_id = 42n
[@inline] let mock_fa2_decimal_digits : nat = 11n

(* in use *)

(* tok *)
[@inline] let tok_token_id : FA2.fa2_token_id = wtez_token_id
[@inline] let tok_decimal_digits : nat = wtez_decimal_digits

(* ctok *)
[@inline] let ctok_token_id : FA2.fa2_token_id = 3n
[@inline] let ctok_decimal_digits : nat = 6n
