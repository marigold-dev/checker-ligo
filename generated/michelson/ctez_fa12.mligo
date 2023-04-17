{ parameter
    (or (or (or (pair %approve (address %spender) (nat %value))
                (pair %getAllowance
                   (pair %request (address %owner) (address %spender))
                   (contract %callback nat)))
            (or (pair %getBalance (address %owner) (contract %callback nat))
                (pair %getTotalSupply (unit %request) (contract %callback nat))))
        (or (pair %mintOrBurn (int %quantity) (address %target))
            (pair %transfer (address %from) (address %to) (nat %value)))) ;
  storage
    (pair (big_map %tokens address nat)
          (big_map %allowances (pair (address %owner) (address %spender)) nat)
          (address %admin)
          (nat %total_supply)) ;
  code { UNPAIR ;
         PUSH mutez 0 ;
         AMOUNT ;
         COMPARE ;
         NEQ ;
         IF { PUSH string "DontSendTez" ; FAILWITH } {} ;
         IF_LEFT
           { IF_LEFT
               { IF_LEFT
                   { DUP 2 ;
                     GET 3 ;
                     DUP 2 ;
                     CAR ;
                     SENDER ;
                     PAIR ;
                     PUSH nat 0 ;
                     DUP 4 ;
                     CDR ;
                     COMPARE ;
                     GT ;
                     PUSH nat 0 ;
                     DUP 4 ;
                     DUP 4 ;
                     GET ;
                     IF_NONE { PUSH nat 0 } {} ;
                     COMPARE ;
                     GT ;
                     AND ;
                     IF { PUSH string "UnsafeAllowanceChange" ; FAILWITH } {} ;
                     DIG 3 ;
                     DIG 3 ;
                     CDR ;
                     DIG 3 ;
                     PUSH nat 0 ;
                     DUP 3 ;
                     COMPARE ;
                     EQ ;
                     IF { SWAP ; DROP ; NONE nat } { SWAP ; SOME } ;
                     DIG 3 ;
                     UPDATE ;
                     UPDATE 3 ;
                     NIL operation }
                   { DUP 2 ;
                     NIL operation ;
                     DUP 3 ;
                     CDR ;
                     PUSH mutez 0 ;
                     DIG 5 ;
                     GET 3 ;
                     DIG 5 ;
                     CAR ;
                     GET ;
                     IF_NONE { PUSH nat 0 } {} ;
                     TRANSFER_TOKENS ;
                     CONS } }
               { IF_LEFT
                   { DUP 2 ;
                     NIL operation ;
                     DUP 3 ;
                     CDR ;
                     PUSH mutez 0 ;
                     DIG 5 ;
                     CAR ;
                     DIG 5 ;
                     CAR ;
                     GET ;
                     IF_NONE { PUSH nat 0 } {} ;
                     TRANSFER_TOKENS }
                   { DUP 2 ;
                     NIL operation ;
                     DIG 2 ;
                     CDR ;
                     PUSH mutez 0 ;
                     DIG 4 ;
                     GET 6 ;
                     TRANSFER_TOKENS } ;
                 CONS } }
           { IF_LEFT
               { DUP 2 ;
                 GET 5 ;
                 SENDER ;
                 COMPARE ;
                 NEQ ;
                 IF { PUSH string "OnlyAdmin" ; FAILWITH } {} ;
                 DUP ;
                 CAR ;
                 DUP 3 ;
                 CAR ;
                 DUP 3 ;
                 CDR ;
                 GET ;
                 IF_NONE { PUSH nat 0 } {} ;
                 ADD ;
                 ISNAT ;
                 IF_NONE
                   { PUSH string "Cannot burn more than the target's balance." ; FAILWITH }
                   {} ;
                 DUP 2 ;
                 CAR ;
                 DUP 4 ;
                 GET 6 ;
                 ADD ;
                 ABS ;
                 DUP 4 ;
                 DIG 4 ;
                 CAR ;
                 PUSH nat 0 ;
                 DUP 5 ;
                 COMPARE ;
                 EQ ;
                 IF { DIG 3 ; DROP ; NONE nat } { DIG 3 ; SOME } ;
                 DIG 4 ;
                 CDR ;
                 UPDATE ;
                 UPDATE 1 ;
                 SWAP ;
                 UPDATE 6 }
               { DUP 2 ;
                 GET 3 ;
                 DUP 3 ;
                 CAR ;
                 DUP 3 ;
                 CAR ;
                 SENDER ;
                 COMPARE ;
                 EQ ;
                 IF { SWAP }
                    { SENDER ;
                      DUP 4 ;
                      CAR ;
                      PAIR ;
                      DUP 4 ;
                      GET 4 ;
                      DUP 4 ;
                      DUP 3 ;
                      GET ;
                      IF_NONE { PUSH nat 0 } {} ;
                      SUB ;
                      ISNAT ;
                      IF_NONE { PUSH string "NotEnoughAllowance" ; FAILWITH } {} ;
                      DIG 3 ;
                      PUSH nat 0 ;
                      DUP 3 ;
                      COMPARE ;
                      EQ ;
                      IF { SWAP ; DROP ; NONE nat } { SWAP ; SOME } ;
                      DIG 2 ;
                      UPDATE } ;
                 DUP 3 ;
                 GET 4 ;
                 DUP 3 ;
                 DUP 5 ;
                 CAR ;
                 GET ;
                 IF_NONE { PUSH nat 0 } {} ;
                 SUB ;
                 ISNAT ;
                 IF_NONE { PUSH string "NotEnoughBalance" ; FAILWITH } {} ;
                 DIG 2 ;
                 PUSH nat 0 ;
                 DUP 3 ;
                 COMPARE ;
                 EQ ;
                 IF { SWAP ; DROP ; NONE nat } { SWAP ; SOME } ;
                 DUP 4 ;
                 CAR ;
                 UPDATE ;
                 DUP 3 ;
                 GET 4 ;
                 DUP 2 ;
                 DUP 5 ;
                 GET 3 ;
                 GET ;
                 IF_NONE { PUSH nat 0 } {} ;
                 ADD ;
                 DIG 4 ;
                 DIG 2 ;
                 PUSH nat 0 ;
                 DUP 4 ;
                 COMPARE ;
                 EQ ;
                 IF { DIG 2 ; DROP ; NONE nat } { DIG 2 ; SOME } ;
                 DIG 4 ;
                 GET 3 ;
                 UPDATE ;
                 UPDATE 1 ;
                 SWAP ;
                 UPDATE 3 } ;
             NIL operation } ;
         PAIR } }

