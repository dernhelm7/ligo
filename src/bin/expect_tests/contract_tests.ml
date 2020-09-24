open Cli_expect

let contract basename =
  "../../test/contracts/" ^ basename
let bad_contract basename =
  "../../test/contracts/negative/" ^ basename

let%expect_test _ =
  run_ligo_good [ "measure-contract" ; contract "coase.ligo" ; "main" ] ;
  [%expect {| 1238 bytes |}] ;

  run_ligo_good [ "measure-contract" ; contract "multisig.ligo" ; "main" ] ;
  [%expect {| 828 bytes |}] ;

  run_ligo_good [ "measure-contract" ; contract "multisig-v2.ligo" ; "main" ] ;
  [%expect {| 1907 bytes |}] ;

  run_ligo_good [ "measure-contract" ; contract "vote.mligo" ; "main" ] ;
  [%expect {| 479 bytes |}] ;

  run_ligo_good [ "measure-contract" ; contract "issue-184-combs.mligo" ; "main2" ] ;
  [%expect {| 295 bytes |}] ;

  run_ligo_good [ "compile-parameter" ; contract "coase.ligo" ; "main" ; "Buy_single (record card_to_buy = 1n end)" ] ;
  [%expect {| (Left (Left 1)) |}] ;

  run_ligo_good [ "compile-storage" ; contract "coase.ligo" ; "main" ; "record cards = (map end : cards) ; card_patterns = (map end : card_patterns) ; next_id = 3n ; end" ] ;
  [%expect {| (Pair (Pair {} {}) 3) |}] ;

  run_ligo_bad [ "compile-storage" ; contract "coase.ligo" ; "main" ; "Buy_single (record card_to_buy = 1n end)" ] ;
  [%expect {|
    ligo: error
          Invalid command line argument.
          The provided storage does not have the correct type for the contract.
          in file "coase.ligo", line 124, characters 9-13
          Invalid type(s).
          Expected: "record[card_patterns -> Map (nat , record[coefficient -> mutez , quantity -> nat]) , cards -> Map (nat , record[card_owner -> address , card_pattern -> nat]) , next_id -> nat]", but got: "
          sum[Buy_single -> record[card_to_buy -> nat] , Sell_single -> record[card_to_sell -> nat] , Transfer_single -> record[card_to_transfer -> nat , destination -> address]]".


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}] ;

  run_ligo_bad [ "compile-parameter" ; contract "coase.ligo" ; "main" ; "record cards = (map end : cards) ; card_patterns = (map end : card_patterns) ; next_id = 3n ; end" ] ;
  [%expect {|
    ligo: error
          Invalid command line argument.
          The provided parameter does not have the correct type for the given entrypoint.
          in file "coase.ligo", line 124, characters 9-13
          Invalid type(s).
          Expected: "sum[Buy_single -> record[card_to_buy -> nat] , Sell_single -> record[card_to_sell -> nat] , Transfer_single -> record[card_to_transfer -> nat , destination -> address]]", but got: "
          record[card_patterns -> Map (nat , record[coefficient -> mutez , quantity -> nat]) , cards -> Map (nat , record[card_owner -> address , card_pattern -> nat]) , next_id -> nat]".


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}] ;

  ()

let%expect_test _  =
  run_ligo_good [ "compile-storage" ; contract "timestamp.ligo" ; "main" ; "now" ; "--now" ; "2042-01-01T00:00:00Z" ] ;
  [%expect {| "2042-01-01T00:00:00Z" |}]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "coase.ligo" ; "main" ] ;
  [%expect {|
{ parameter
    (or (or (nat %buy_single) (nat %sell_single))
        (pair %transfer_single (nat %card_to_transfer) (address %destination))) ;
  storage
    (pair (pair (map %card_patterns nat (pair (mutez %coefficient) (nat %quantity)))
                (map %cards nat (pair (address %card_owner) (nat %card_pattern))))
          (nat %next_id)) ;
  code { DUP ;
         CDR ;
         SWAP ;
         CAR ;
         IF_LEFT
           { IF_LEFT
               { SWAP ;
                 DUP ;
                 CAR ;
                 CAR ;
                 DIG 2 ;
                 DUP ;
                 DUG 3 ;
                 GET ;
                 IF_NONE { PUSH string "buy_single: No card pattern." ; FAILWITH } {} ;
                 PUSH nat 1 ;
                 SWAP ;
                 DUP ;
                 DUG 2 ;
                 CDR ;
                 ADD ;
                 SWAP ;
                 DUP ;
                 DUG 2 ;
                 CAR ;
                 MUL ;
                 AMOUNT ;
                 SWAP ;
                 COMPARE ;
                 GT ;
                 IF { PUSH string "Not enough money" ; FAILWITH } { PUSH unit Unit } ;
                 DROP ;
                 DUP ;
                 PUSH nat 1 ;
                 DIG 2 ;
                 CDR ;
                 ADD ;
                 SWAP ;
                 CAR ;
                 PAIR ;
                 SWAP ;
                 DUP ;
                 CAR ;
                 CAR ;
                 DIG 2 ;
                 DIG 3 ;
                 DUP ;
                 DUG 4 ;
                 SWAP ;
                 SOME ;
                 SWAP ;
                 UPDATE ;
                 DIP { DUP ; CDR ; SWAP ; CAR ; CDR } ;
                 PAIR ;
                 PAIR ;
                 DUP ;
                 CAR ;
                 CDR ;
                 DIG 2 ;
                 SENDER ;
                 PAIR ;
                 DIG 2 ;
                 DUP ;
                 DUG 3 ;
                 CDR ;
                 SWAP ;
                 SOME ;
                 SWAP ;
                 UPDATE ;
                 DIP { DUP ; CDR ; SWAP ; CAR ; CAR } ;
                 SWAP ;
                 PAIR ;
                 PAIR ;
                 DUP ;
                 PUSH nat 1 ;
                 DIG 2 ;
                 CDR ;
                 ADD ;
                 SWAP ;
                 CAR ;
                 PAIR ;
                 NIL operation ;
                 PAIR }
               { SWAP ;
                 DUP ;
                 CAR ;
                 CDR ;
                 DIG 2 ;
                 DUP ;
                 DUG 3 ;
                 GET ;
                 IF_NONE { PUSH string "sell_single: No card." ; FAILWITH } {} ;
                 SENDER ;
                 SWAP ;
                 DUP ;
                 DUG 2 ;
                 CAR ;
                 COMPARE ;
                 NEQ ;
                 IF { PUSH string "This card doesn't belong to you" ; FAILWITH }
                    { PUSH unit Unit } ;
                 DROP ;
                 SWAP ;
                 DUP ;
                 DUG 2 ;
                 CAR ;
                 CAR ;
                 SWAP ;
                 DUP ;
                 DUG 2 ;
                 CDR ;
                 GET ;
                 IF_NONE { PUSH string "sell_single: No card pattern." ; FAILWITH } {} ;
                 DUP ;
                 PUSH nat 1 ;
                 DIG 2 ;
                 CDR ;
                 SUB ;
                 ABS ;
                 SWAP ;
                 CAR ;
                 PAIR ;
                 DIG 2 ;
                 DUP ;
                 CAR ;
                 CAR ;
                 DIG 2 ;
                 DUP ;
                 DUG 3 ;
                 DIG 4 ;
                 CDR ;
                 SWAP ;
                 SOME ;
                 SWAP ;
                 UPDATE ;
                 DIP { DUP ; CDR ; SWAP ; CAR ; CDR } ;
                 PAIR ;
                 PAIR ;
                 SWAP ;
                 DUP ;
                 DUG 2 ;
                 CDR ;
                 DIG 2 ;
                 CAR ;
                 MUL ;
                 SENDER ;
                 CONTRACT unit ;
                 IF_NONE { PUSH string "sell_single: No contract." ; FAILWITH } {} ;
                 SWAP ;
                 UNIT ;
                 TRANSFER_TOKENS ;
                 SWAP ;
                 DUP ;
                 CAR ;
                 CDR ;
                 DIG 3 ;
                 NONE (pair address nat) ;
                 SWAP ;
                 UPDATE ;
                 DIP { DUP ; CDR ; SWAP ; CAR ; CAR } ;
                 SWAP ;
                 PAIR ;
                 PAIR ;
                 NIL operation ;
                 DIG 2 ;
                 CONS ;
                 PAIR } }
           { SWAP ;
             DUP ;
             CAR ;
             CDR ;
             DUP ;
             DIG 3 ;
             DUP ;
             DUG 4 ;
             CAR ;
             GET ;
             IF_NONE { PUSH string "transfer_single: No card." ; FAILWITH } {} ;
             SENDER ;
             SWAP ;
             DUP ;
             DUG 2 ;
             CAR ;
             COMPARE ;
             NEQ ;
             IF { PUSH string "This card doesn't belong to you" ; FAILWITH }
                { PUSH unit Unit } ;
             DROP ;
             DIG 3 ;
             DUP ;
             DUG 4 ;
             CDR ;
             SWAP ;
             CDR ;
             SWAP ;
             PAIR ;
             DIG 3 ;
             CAR ;
             SWAP ;
             SOME ;
             SWAP ;
             UPDATE ;
             DIP { DUP ; CDR ; SWAP ; CAR ; CAR } ;
             SWAP ;
             PAIR ;
             PAIR ;
             NIL operation ;
             PAIR } } } |} ]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "multisig.ligo" ; "main" ] ;
  [%expect {|
{ parameter
    (pair (pair (nat %counter) (lambda %message unit (list operation)))
          (list %signatures (pair key_hash signature))) ;
  storage
    (pair (pair (list %auth key) (nat %counter)) (pair (string %id) (nat %threshold))) ;
  code { DUP ;
         CAR ;
         SWAP ;
         CDR ;
         SWAP ;
         DUP ;
         DUG 2 ;
         CAR ;
         CDR ;
         SWAP ;
         DUP ;
         DUG 2 ;
         CAR ;
         CDR ;
         DIG 3 ;
         DUP ;
         DUG 4 ;
         CAR ;
         CAR ;
         COMPARE ;
         NEQ ;
         IF { DIG 2 ; DROP ; PUSH string "Counters does not match" ; FAILWITH }
            { CHAIN_ID ;
              DIG 2 ;
              DUP ;
              DUG 3 ;
              CDR ;
              CAR ;
              PAIR ;
              DIG 3 ;
              DUP ;
              DUG 4 ;
              CAR ;
              CAR ;
              DIG 2 ;
              DUP ;
              DUG 3 ;
              PAIR ;
              PAIR ;
              PACK ;
              PUSH nat 0 ;
              DIG 3 ;
              DUP ;
              DUG 4 ;
              CAR ;
              CAR ;
              PAIR ;
              DIG 4 ;
              CDR ;
              ITER { SWAP ;
                     PAIR ;
                     DUP ;
                     CAR ;
                     CDR ;
                     SWAP ;
                     DUP ;
                     DUG 2 ;
                     CAR ;
                     CAR ;
                     DIG 2 ;
                     DUP ;
                     DUG 3 ;
                     CDR ;
                     DIG 2 ;
                     DUP ;
                     DUG 3 ;
                     DIG 2 ;
                     DUP ;
                     DUG 3 ;
                     PAIR ;
                     DIG 2 ;
                     IF_CONS
                       { DUP ;
                         DUG 2 ;
                         HASH_KEY ;
                         DIG 4 ;
                         DUP ;
                         DUG 5 ;
                         CAR ;
                         COMPARE ;
                         EQ ;
                         IF { DIG 6 ;
                              DUP ;
                              DUG 7 ;
                              DIG 4 ;
                              CDR ;
                              DIG 3 ;
                              CHECK_SIGNATURE ;
                              IF { PUSH nat 1 ;
                                   DIG 3 ;
                                   DUP ;
                                   DUG 4 ;
                                   ADD ;
                                   DIG 3 ;
                                   DUP ;
                                   DUG 4 ;
                                   SWAP ;
                                   DIP { DROP } }
                                 { PUSH string "Invalid signature" ; FAILWITH } ;
                              DIG 3 ;
                              SWAP ;
                              DIP { DROP } }
                            { SWAP ; DROP ; DIG 2 ; DROP ; DIG 2 } ;
                         DUG 2 ;
                         SWAP ;
                         CDR ;
                         SWAP ;
                         PAIR ;
                         CAR ;
                         PAIR }
                       { SWAP ; DROP ; SWAP ; DROP } ;
                     DUP ;
                     DUG 2 ;
                     CAR ;
                     DIP { DUP ; CDR ; SWAP ; CAR ; CDR } ;
                     PAIR ;
                     PAIR ;
                     SWAP ;
                     CDR ;
                     DIP { DUP ; CDR ; SWAP ; CAR ; CAR } ;
                     SWAP ;
                     PAIR ;
                     PAIR ;
                     CAR } ;
              SWAP ;
              DROP ;
              DIG 2 ;
              DUP ;
              DUG 3 ;
              CDR ;
              CDR ;
              SWAP ;
              CDR ;
              COMPARE ;
              LT ;
              IF { PUSH string "Not enough signatures passed the check" ; FAILWITH }
                 { SWAP ;
                   DUP ;
                   DUG 2 ;
                   PUSH nat 1 ;
                   DIG 3 ;
                   DUP ;
                   DUG 4 ;
                   CAR ;
                   CDR ;
                   ADD ;
                   DIP { DUP ; CDR ; SWAP ; CAR ; CAR } ;
                   SWAP ;
                   PAIR ;
                   PAIR ;
                   DIG 2 ;
                   DUP ;
                   DUG 3 ;
                   SWAP ;
                   DIP { DROP } } ;
              DIG 2 ;
              SWAP ;
              DIP { DROP } } ;
         UNIT ;
         DIG 2 ;
         SWAP ;
         EXEC ;
         PAIR } } |} ]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "multisig-v2.ligo" ; "main" ] ;
  [%expect {|
{ parameter
    (or (or (unit %default) (lambda %send bytes (list operation)))
        (lambda %withdraw bytes (list operation))) ;
  storage
    (pair (pair (pair (set %authorized_addresses address) (nat %max_message_size))
                (pair (nat %max_proposal) (map %message_store bytes (set address))))
          (pair (pair (map %proposal_counters address nat) (bytes %state_hash))
                (nat %threshold))) ;
  code { DUP ;
         CDR ;
         SWAP ;
         CAR ;
         IF_LEFT
           { IF_LEFT
               { DROP ; NIL operation ; PAIR }
               { PAIR ;
                 DUP ;
                 CDR ;
                 DUP ;
                 CAR ;
                 CAR ;
                 CAR ;
                 SENDER ;
                 MEM ;
                 NOT ;
                 IF { PUSH string "Unauthorized address" ; FAILWITH } { PUSH unit Unit } ;
                 DROP ;
                 SWAP ;
                 CAR ;
                 DUP ;
                 PACK ;
                 DIG 2 ;
                 DUP ;
                 DUG 3 ;
                 CAR ;
                 CAR ;
                 CDR ;
                 SWAP ;
                 DUP ;
                 DUG 2 ;
                 SIZE ;
                 COMPARE ;
                 GT ;
                 IF { PUSH string "Message size exceed maximum limit" ; FAILWITH }
                    { PUSH unit Unit } ;
                 DROP ;
                 DIG 2 ;
                 DUP ;
                 DUG 3 ;
                 EMPTY_SET address ;
                 PAIR ;
                 DIG 3 ;
                 DUP ;
                 DUG 4 ;
                 CAR ;
                 CDR ;
                 CDR ;
                 DIG 2 ;
                 DUP ;
                 DUG 3 ;
                 GET ;
                 IF_NONE
                   { DIG 3 ;
                     DUP ;
                     DUP ;
                     DUG 5 ;
                     CDR ;
                     CAR ;
                     CAR ;
                     PUSH nat 1 ;
                     DIG 6 ;
                     CDR ;
                     CAR ;
                     CAR ;
                     SENDER ;
                     GET ;
                     IF_NONE { PUSH string "MAP FIND" ; FAILWITH } {} ;
                     ADD ;
                     SOME ;
                     SENDER ;
                     UPDATE ;
                     DIP { DUP ; CAR ; SWAP ; CDR ; DUP ; CDR ; SWAP ; CAR ; CDR } ;
                     PAIR ;
                     PAIR ;
                     SWAP ;
                     PAIR ;
                     EMPTY_SET address ;
                     PUSH bool True ;
                     SENDER ;
                     UPDATE ;
                     DUG 2 ;
                     SWAP ;
                     CAR ;
                     PAIR ;
                     CDR ;
                     SWAP ;
                     PAIR }
                   { DUP ;
                     SENDER ;
                     MEM ;
                     IF { DIG 4 }
                        { DIG 4 ;
                          DUP ;
                          DUP ;
                          DUG 6 ;
                          CDR ;
                          CAR ;
                          CAR ;
                          PUSH nat 1 ;
                          DIG 7 ;
                          DUP ;
                          DUG 8 ;
                          CDR ;
                          CAR ;
                          CAR ;
                          SENDER ;
                          GET ;
                          IF_NONE { PUSH string "MAP FIND" ; FAILWITH } {} ;
                          ADD ;
                          SOME ;
                          SENDER ;
                          UPDATE ;
                          DIP { DUP ; CAR ; SWAP ; CDR ; DUP ; CDR ; SWAP ; CAR ; CDR } ;
                          PAIR ;
                          PAIR ;
                          SWAP ;
                          PAIR ;
                          DIG 5 ;
                          SWAP ;
                          DIP { DROP } } ;
                     SWAP ;
                     PUSH bool True ;
                     SENDER ;
                     UPDATE ;
                     DUG 2 ;
                     SWAP ;
                     CAR ;
                     PAIR ;
                     CDR ;
                     SWAP ;
                     PAIR } ;
                 DUP ;
                 CAR ;
                 SWAP ;
                 CDR ;
                 DUP ;
                 CDR ;
                 CAR ;
                 CAR ;
                 SENDER ;
                 GET ;
                 IF_NONE { PUSH string "MAP FIND" ; FAILWITH } {} ;
                 SWAP ;
                 DUP ;
                 DUG 2 ;
                 CAR ;
                 CDR ;
                 CAR ;
                 SWAP ;
                 COMPARE ;
                 GT ;
                 IF { PUSH string "Maximum number of proposal reached" ; FAILWITH }
                    { PUSH unit Unit } ;
                 DROP ;
                 DUP ;
                 NIL operation ;
                 PAIR ;
                 SWAP ;
                 DUP ;
                 DUG 2 ;
                 CDR ;
                 CDR ;
                 DIG 3 ;
                 DUP ;
                 DUG 4 ;
                 SIZE ;
                 COMPARE ;
                 GE ;
                 IF { SWAP ;
                      DUP ;
                      CAR ;
                      CDR ;
                      CDR ;
                      DIG 4 ;
                      DUP ;
                      DUG 5 ;
                      NONE (set address) ;
                      SWAP ;
                      UPDATE ;
                      DIP { DUP ; CDR ; SWAP ; CAR ; DUP ; CAR ; SWAP ; CDR ; CAR } ;
                      SWAP ;
                      PAIR ;
                      SWAP ;
                      PAIR ;
                      PAIR ;
                      DUP ;
                      CDR ;
                      CAR ;
                      CDR ;
                      DIG 5 ;
                      SWAP ;
                      EXEC ;
                      SWAP ;
                      DUP ;
                      DUG 2 ;
                      DIG 5 ;
                      DIG 3 ;
                      DUP ;
                      DUG 4 ;
                      CDR ;
                      CAR ;
                      CDR ;
                      CONCAT ;
                      SHA256 ;
                      DIP { DUP ; CAR ; SWAP ; CDR ; DUP ; CDR ; SWAP ; CAR ; CAR } ;
                      SWAP ;
                      PAIR ;
                      PAIR ;
                      SWAP ;
                      PAIR ;
                      DUP ;
                      DUP ;
                      DUG 2 ;
                      CDR ;
                      CAR ;
                      CAR ;
                      ITER { SWAP ;
                             PAIR ;
                             DUP ;
                             CAR ;
                             SWAP ;
                             DUP ;
                             DUG 2 ;
                             CDR ;
                             CAR ;
                             DIG 7 ;
                             DUP ;
                             DUG 8 ;
                             SWAP ;
                             DUP ;
                             DUG 2 ;
                             MEM ;
                             IF { SWAP ;
                                  DUP ;
                                  DUP ;
                                  DUG 3 ;
                                  CDR ;
                                  CAR ;
                                  CAR ;
                                  PUSH nat 1 ;
                                  DIG 5 ;
                                  DUP ;
                                  DUG 6 ;
                                  CDR ;
                                  CDR ;
                                  SUB ;
                                  ABS ;
                                  DIG 3 ;
                                  SWAP ;
                                  SOME ;
                                  SWAP ;
                                  UPDATE ;
                                  DIP { DUP ; CAR ; SWAP ; CDR ; DUP ; CDR ; SWAP ; CAR ; CDR } ;
                                  PAIR ;
                                  PAIR ;
                                  SWAP ;
                                  PAIR ;
                                  DIP { DROP } }
                                { DROP } ;
                             SWAP ;
                             CDR ;
                             SWAP ;
                             PAIR ;
                             CAR } ;
                      DIG 5 ;
                      DROP ;
                      DIG 4 ;
                      DIG 4 ;
                      SWAP ;
                      CAR ;
                      PAIR ;
                      DIG 3 ;
                      SWAP ;
                      CDR ;
                      SWAP ;
                      PAIR ;
                      DIG 2 ;
                      SWAP ;
                      CAR ;
                      PAIR ;
                      CAR ;
                      PAIR }
                    { DIG 4 ;
                      DROP ;
                      SWAP ;
                      DUP ;
                      CAR ;
                      CDR ;
                      CDR ;
                      DIG 3 ;
                      DIG 4 ;
                      SWAP ;
                      SOME ;
                      SWAP ;
                      UPDATE ;
                      DIP { DUP ; CDR ; SWAP ; CAR ; DUP ; CAR ; SWAP ; CDR ; CAR } ;
                      SWAP ;
                      PAIR ;
                      SWAP ;
                      PAIR ;
                      PAIR ;
                      SWAP ;
                      CAR ;
                      PAIR } } }
           { PACK ;
             SWAP ;
             DUP ;
             DUG 2 ;
             CAR ;
             CDR ;
             CDR ;
             SWAP ;
             DUP ;
             DUG 2 ;
             GET ;
             IF_NONE
               { DROP }
               { DUP ;
                 PUSH bool False ;
                 SENDER ;
                 UPDATE ;
                 DUP ;
                 SIZE ;
                 DIG 2 ;
                 SIZE ;
                 COMPARE ;
                 NEQ ;
                 IF { DIG 2 ;
                      DUP ;
                      DUP ;
                      DUG 4 ;
                      CDR ;
                      CAR ;
                      CAR ;
                      PUSH nat 1 ;
                      DIG 5 ;
                      DUP ;
                      DUG 6 ;
                      CDR ;
                      CAR ;
                      CAR ;
                      SENDER ;
                      GET ;
                      IF_NONE { PUSH string "MAP FIND" ; FAILWITH } {} ;
                      SUB ;
                      ABS ;
                      SOME ;
                      SENDER ;
                      UPDATE ;
                      DIP { DUP ; CAR ; SWAP ; CDR ; DUP ; CDR ; SWAP ; CAR ; CDR } ;
                      PAIR ;
                      PAIR ;
                      SWAP ;
                      PAIR ;
                      DIG 3 ;
                      DUP ;
                      DUG 4 ;
                      SWAP ;
                      DIP { DROP } }
                    { DIG 2 ; DUP ; DUG 3 } ;
                 DIG 3 ;
                 SWAP ;
                 DUP ;
                 DUG 2 ;
                 DIP { DROP } ;
                 PUSH nat 0 ;
                 DIG 3 ;
                 DUP ;
                 DUG 4 ;
                 SIZE ;
                 COMPARE ;
                 EQ ;
                 IF { DIG 2 ;
                      DROP ;
                      SWAP ;
                      DUP ;
                      DUP ;
                      CAR ;
                      CDR ;
                      CDR ;
                      DIG 4 ;
                      NONE (set address) ;
                      SWAP ;
                      UPDATE ;
                      DIP { DUP ; CDR ; SWAP ; CAR ; DUP ; CAR ; SWAP ; CDR ; CAR } ;
                      SWAP ;
                      PAIR ;
                      SWAP ;
                      PAIR ;
                      PAIR ;
                      DIP { DROP } }
                    { SWAP ;
                      DUP ;
                      DUP ;
                      CAR ;
                      CDR ;
                      CDR ;
                      DIG 4 ;
                      DIG 5 ;
                      SWAP ;
                      SOME ;
                      SWAP ;
                      UPDATE ;
                      DIP { DUP ; CDR ; SWAP ; CAR ; DUP ; CAR ; SWAP ; CDR ; CAR } ;
                      SWAP ;
                      PAIR ;
                      SWAP ;
                      PAIR ;
                      PAIR ;
                      DIP { DROP } } ;
                 DIP { DROP } } ;
             NIL operation ;
             PAIR } } } |} ]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "vote.mligo" ; "main" ] ;
  [%expect {|
{ parameter
    (or (pair %reset (pair (timestamp %finish_time) (timestamp %start_time)) (string %title))
        (or %vote (unit %nay) (unit %yea))) ;
  storage
    (pair (pair (pair (timestamp %finish_time) (nat %nay))
                (pair (timestamp %start_time) (string %title)))
          (pair (set %voters address) (nat %yea))) ;
  code { DUP ;
         CAR ;
         IF_LEFT
           { SWAP ;
             DROP ;
             PUSH nat 0 ;
             EMPTY_SET address ;
             PAIR ;
             SWAP ;
             DUP ;
             DUG 2 ;
             CDR ;
             DIG 2 ;
             DUP ;
             DUG 3 ;
             CAR ;
             CDR ;
             PAIR ;
             PUSH nat 0 ;
             DIG 3 ;
             CAR ;
             CAR ;
             PAIR ;
             PAIR ;
             PAIR ;
             NIL operation ;
             PAIR }
           { SWAP ;
             CDR ;
             SWAP ;
             PAIR ;
             DUP ;
             CDR ;
             SENDER ;
             DIG 2 ;
             CAR ;
             IF_LEFT
               { DROP ;
                 SWAP ;
                 DUP ;
                 DUG 2 ;
                 PUSH nat 1 ;
                 DIG 3 ;
                 CAR ;
                 CAR ;
                 CDR ;
                 ADD ;
                 DIP { DUP ; CDR ; SWAP ; CAR ; DUP ; CDR ; SWAP ; CAR ; CAR } ;
                 SWAP ;
                 PAIR ;
                 PAIR ;
                 PAIR }
               { DROP ;
                 SWAP ;
                 DUP ;
                 DUG 2 ;
                 PUSH nat 1 ;
                 DIG 3 ;
                 CDR ;
                 CDR ;
                 ADD ;
                 DIP { DUP ; CAR ; SWAP ; CDR ; CAR } ;
                 SWAP ;
                 PAIR ;
                 SWAP ;
                 PAIR } ;
             DUP ;
             CDR ;
             CAR ;
             DIG 2 ;
             PUSH bool True ;
             SWAP ;
             UPDATE ;
             DIP { DUP ; CAR ; SWAP ; CDR ; CDR } ;
             PAIR ;
             SWAP ;
             PAIR ;
             NIL operation ;
             PAIR } } } |}]

let%expect_test _ =
    run_ligo_good [ "compile-contract" ; contract "implicit.mligo" ; "main" ] ;
    [%expect {|
      { parameter key_hash ;
        storage unit ;
        code { DROP ; UNIT ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "amount_lambda.mligo" ; "main" ] ;
  (* AMOUNT should occur inside the second lambda, but not the first lambda *)
  [%expect {|
    { parameter bool ;
      storage (lambda unit mutez) ;
      code { CAR ;
             IF { AMOUNT ; LAMBDA (pair mutez unit) mutez { CAR } ; SWAP ; APPLY }
                { LAMBDA unit mutez { DROP ; AMOUNT } } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "print-ast-typed" ; contract "sequence.mligo" ; ];
  [%expect {| const y = lambda (#1) return let x = +1 in let _ = let x = +2 in UNIT() in let _ = let x = +23 in UNIT() in let _ = let x = +42 in UNIT() in x |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; contract "bad_type_operator.ligo" ; "main" ] ;
  [%expect {|
    ligo: error
          in file "bad_type_operator.ligo", line 4, characters 16-29
          Wrong number of arguments for type constant: Map
          expected: 2
          got: 1


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; contract "bad_address_format.religo" ; "main" ] ;
  [%expect {|
    ligo: error
          Error(s) occurred while type checking the contract:
          Ill typed contract:
            1: { parameter int ;
            2:   storage address ;
            3:   code { DROP /* [] */ ; PUSH address "KT1badaddr" ; NIL operation ; PAIR } }
          Invalid contract notation "KT1badaddr"


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; contract "bad_timestamp.ligo" ; "main" ] ;
  [%expect {|
    ligo: error
          in file "bad_timestamp.ligo", line 7, characters 30-44
          Ill-formed timestamp "badtimestamp".
          At this point, a string with a RFC3339 notation or the number of seconds since Epoch is expected.


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}]

let%expect_test _ =
    run_ligo_good [ "dry-run" ; contract "redeclaration.ligo" ; "main" ; "unit" ; "0" ] ;
    [%expect {|( LIST_EMPTY() , 0 ) |}]

let%expect_test _ =
    run_ligo_good [ "dry-run" ; contract "double_main.ligo" ; "main" ; "unit" ; "0" ] ;
    [%expect {|( LIST_EMPTY() , 2 ) |}]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "subtle_nontail_fail.mligo" ; "main" ] ;
  [%expect {|
    { parameter unit ;
      storage unit ;
      code { DROP ;
             PUSH bool True ;
             IF { PUSH string "This contract always fails" ; FAILWITH }
                { PUSH string "This contract still always fails" ; FAILWITH } } } |}]

let%expect_test _ =
  (* TODO should not be bad? *)
  run_ligo_good [ "dry-run" ; contract "subtle_nontail_fail.mligo" ; "main" ; "()" ; "()" ] ;
  [%expect {|
    failwith("This contract always fails") |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; bad_contract "self_in_lambda.mligo" ; "main" ] ;
  [%expect {|
    ligo: error
          "Tezos.self_address" must be used directly and cannot be used via another function.


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}]

let%expect_test _ =
  run_ligo_good [ "compile-storage" ; contract "big_map.ligo" ; "main" ; "(big_map1,unit)" ] ;
  [%expect {|
    (Pair { Elt 23 0 ; Elt 42 0 } Unit) |}]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "key_hash_comparable.ligo" ; "main" ] ;
  [%expect {|
    { parameter int ;
      storage (pair (map %one key_hash nat) (big_map %two key_hash bool)) ;
      code { CDR ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; bad_contract "long_sum_type_names.ligo" ; "main" ] ;
  [%expect {|
    ligo: error
          in file "long_sum_type_names.ligo", line 2, character 2 to line 4, character 18
          Ill-formed data constructor "Incrementttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt".
          Data constructors have a maximum length of 32 characters, which is a limitation imposed by annotations in Tezos.


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}]

let%expect_test _ =
  run_ligo_good [ "dry-run" ; contract "super-counter.mligo" ; "main" ; "test_param" ; "test_storage" ] ;
  [%expect {|
    ( LIST_EMPTY() , 3 ) |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; bad_contract "redundant_constructors.mligo" ; "main" ] ;
  [%expect{|
    ligo: error
          in file "redundant_constructors.mligo", line 7, character 2 to line 9, character 15
          Invalid variant.
          Constructor "Add" already exists as part of another variant.


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; bad_contract "create_contract_toplevel.mligo" ; "main" ] ;
  [%expect {|
ligo: error
      in file "create_contract_toplevel.mligo", line 4, character 35 to line 8, character 8
      Free variable 'store' is not allowed in CREATE_CONTRACT lambda


      If you're not sure how to fix this error, you can do one of the following:

      * Visit our documentation: https://ligolang.org/docs/intro/introduction
      * Ask a question on our Discord: https://discord.gg/9rhYaEt
      * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
      * Check the changelog by running 'ligo changelog' |}] ;

  run_ligo_bad [ "compile-contract" ; bad_contract "create_contract_var.mligo" ; "main" ] ;
  [%expect {|
ligo: error
      in file "create_contract_var.mligo", line 6, character 35 to line 10, character 5
      Free variable 'a' is not allowed in CREATE_CONTRACT lambda


      If you're not sure how to fix this error, you can do one of the following:

      * Visit our documentation: https://ligolang.org/docs/intro/introduction
      * Ask a question on our Discord: https://discord.gg/9rhYaEt
      * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
      * Check the changelog by running 'ligo changelog' |}] ;

  run_ligo_bad [ "compile-contract" ; bad_contract "create_contract_no_inline.mligo" ; "main" ] ;
  [%expect {|
    ligo: error
          in file "create_contract_no_inline.mligo", line 3, characters 40-46
          Type "return" not found.


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}] ;

  run_ligo_good [ "compile-contract" ; contract "create_contract.mligo" ; "main" ] ;
  [%expect {|
    { parameter string ;
      storage string ;
      code { PUSH string "un" ;
             PUSH mutez 300000000 ;
             NONE key_hash ;
             CREATE_CONTRACT
               { parameter nat ;
                 storage string ;
                 code { DROP ; PUSH string "one" ; NIL operation ; PAIR } } ;
             PAIR ;
             SWAP ;
             CDR ;
             NIL operation ;
             DIG 2 ;
             CAR ;
             CONS ;
             PAIR } } |}];

  run_ligo_good [ "compile-contract" ; contract "tuples_no_annotation.religo" ; "main" ] ;
  [%expect {|
    { parameter int ;        
      storage (pair (pair int string) (pair nat bool)) ;
      code { DROP ;
             PUSH bool False ;
             PUSH nat 2 ;
             PAIR ;
             PUSH string "2" ;
             PUSH int 2 ;
             PAIR ;
             PAIR ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; bad_contract "self_type_annotation.ligo" ; "main" ] ;
  [%expect {|
    ligo: error
          in file "self_type_annotation.ligo", line 8, characters 41-64
          Invalid type annotation.
          "Contract (nat)" was given, but "Contract (int)" was expected.
          Note that "Tezos.self" refers to this contract, so the parameters should be the same.


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}] ;

  run_ligo_good [ "compile-contract" ; contract "self_type_annotation.ligo" ; "main" ] ;
  [%expect {|
    { parameter nat ; storage int ; code { CDR ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; bad_contract "bad_contract.mligo" ; "main" ] ;
  [%expect {|
    ligo: error
          in file "bad_contract.mligo", line 4, characters 9-46
          Invalid type for entrypoint "main".
          An entrypoint must of type "parameter * storage -> operations list * storage".


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}] ;

  run_ligo_bad [ "compile-contract" ; bad_contract "bad_contract2.mligo" ; "main" ] ;
  [%expect {|
    ligo: error
          in file "bad_contract2.mligo", line 5, characters 9-46
          Invalid type for entrypoint "main".
          An entrypoint must of type "parameter * storage -> operations list * storage".


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}] ;

  run_ligo_bad [ "compile-contract" ; bad_contract "bad_contract3.mligo" ; "main" ] ;
  [%expect {|
    ligo: error
          in file "bad_contract3.mligo", line 5, characters 9-46
          Invalid type for entrypoint "main".
          The storage type "int" of the function parameter must be the same as the storage type "string" of the return value.


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "self_with_entrypoint.ligo" ; "main" ] ;
  [%expect {|
    { parameter (or (unit %default) (int %toto)) ;
      storage nat ;
      code { SELF %toto ;
             PUSH mutez 300000000 ;
             PUSH int 2 ;
             TRANSFER_TOKENS ;
             SWAP ;
             CDR ;
             NIL operation ;
             DIG 2 ;
             CONS ;
             PAIR } } |}] ;

  run_ligo_good [ "compile-contract" ; contract "self_without_entrypoint.ligo" ; "main" ] ;
  [%expect {|
    { parameter int ;
      storage nat ;
      code { SELF %default ;
             PUSH mutez 300000000 ;
             PUSH int 2 ;
             TRANSFER_TOKENS ;
             SWAP ;
             CDR ;
             NIL operation ;
             DIG 2 ;
             CONS ;
             PAIR } } |}] ;

  run_ligo_bad [ "compile-contract" ; bad_contract "self_bad_entrypoint_format.ligo" ; "main" ] ;
  [%expect {|
    ligo: error
          in file "self_bad_entrypoint_format.ligo", line 8, characters 52-58
          Invalid entrypoint "Toto".
          One of the following patterns is expected:
            * "%bar" is expected for entrypoint "Bar"
            * "%default" when no entrypoint is used.


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}];

  run_ligo_bad ["compile-contract"; bad_contract "nested_bigmap_1.religo"; "main"];
  [%expect {|
    ligo: error
          in file "nested_bigmap_1.religo", line 1, characters 11-29
          Invalid big map nesting.
          A big map cannot be nested inside another big map.


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}];

  run_ligo_bad ["compile-contract"; bad_contract "nested_bigmap_2.religo"; "main"];
  [%expect {|
    ligo: error
          in file "nested_bigmap_2.religo", line 2, characters 29-50
          Invalid big map nesting.
          A big map cannot be nested inside another big map.


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}];
  
  run_ligo_bad ["compile-contract"; bad_contract "nested_bigmap_3.religo"; "main"];
  [%expect {|
    ligo: error
          in file "nested_bigmap_3.religo", line 1, characters 11-29
          Invalid big map nesting.
          A big map cannot be nested inside another big map.


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}];

  run_ligo_bad ["compile-contract"; bad_contract "nested_bigmap_4.religo"; "main"];
  [%expect {|
    ligo: error
          in file "nested_bigmap_4.religo", line 2, characters 39-60
          Invalid big map nesting.
          A big map cannot be nested inside another big map.


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}];
      
  run_ligo_good ["print-ast"; contract "letin.mligo"];
  [%expect {|
type storage = (int ,
int)
const (main : (int ,
storage) -> (list (operation) ,
storage)) = lambda (n:(int ,
storage)) return {let (x : (int ,
int)) = let (x : int) = 7 in (ADD(x ,
n.0) ,
ADD(n.1.0 ,
n.1.1)) in ({list[] : list (operation)} ,
x) : (list (operation) ,
storage)}
const (f0 : _) = lambda (a:string) return true(unit)
const (f1 : _) = lambda (a:string) return true(unit)
const (f2 : _) = lambda (a:string) return true(unit)
const (letin_nesting : _) = lambda (#1:unit) return let (s : _) = "test" in let (p0 : _) = (f0)@(s) in { ASSERTION(p0);
 let (p1 : _) = (f1)@(s) in { ASSERTION(p1);
 let (p2 : _) = (f2)@(s) in { ASSERTION(p2);
 s}}}
const (letin_nesting2 : _) = lambda (x:int) return let (y : _) = 2 in let (z : _) = 3 in ADD(ADD(x ,
y) , z) const (x : _) = let (#5 : _) = (+1 , (+2 ,
+3)) in let (#4 : _) = #5.0 in let (#3 : (_ ,
_)) = #5.1 in let (x : _) = #3.0 in let (#2 : _) = #3.1 in x
    |}];

  run_ligo_good ["print-ast"; contract "letin.religo"];
  [%expect {|
type storage = (int ,
int)
const (main : _) = lambda (n:(int ,
storage)) return {let (x : (int ,
int)) = {let (x : int) = {7 : int} in (ADD(x ,
n.0) ,
ADD(n.1.0 ,
n.1.1)) : (int ,
int)} in ({list[] : list (operation)} ,
x) : (list (operation) ,
storage)}
const (f0 : _) = lambda (a:string) return true(unit)
const (f1 : _) = lambda (a:string) return true(unit)
const (f2 : _) = lambda (a:string) return true(unit)
const (letin_nesting : _) = lambda (_:unit) return let (s : _) = "test" in let (p0 : _) = (f0)@(s) in { ASSERTION(p0);
 let (p1 : _) = (f1)@(s) in { ASSERTION(p1);
 let (p2 : _) = (f2)@(s) in { ASSERTION(p2);
 s}}}
const (letin_nesting2 : _) = lambda (x:int) return let (y : _) = 2 in let (z : _) = 3 in ADD(ADD(x ,
y) , z) const (x : _) = let (#4 : _) = (+1 , (+2 ,
+3)) in let (#3 : _) = #4.0 in let (#2 : (_ ,
_)) = #4.1 in let (x : _) = #2.0 in let (#1 : _) = #2.1 in x
    |}];

  run_ligo_bad ["print-ast-typed"; contract "existential.mligo"];
  [%expect {|
    ligo: error
          Lexical error in file "existential.mligo", line 1, characters 8-9:
          Unexpected character '\''.



          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}];
  run_ligo_bad ["print-ast-typed"; bad_contract "missing_funarg_annotation.mligo"];
  [%expect {|
    ligo: error
          in file "missing_funarg_annotation.mligo", line 2, characters 6-7
          Missing a type annotation for argument "b".


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}];
  run_ligo_bad ["print-ast-typed"; bad_contract "missing_funarg_annotation.religo"];
  [%expect {|
    ligo: error
          in file "missing_funarg_annotation.religo", line 2, characters 8-9
          Missing a type annotation for argument "b".


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}];
  run_ligo_bad ["print-ast-typed"; bad_contract "funarg_tuple_wrong.mligo"];
  [%expect {|
    ligo: error
          in file "funarg_tuple_wrong.mligo", line 1, characters 7-14
          The tuple "b, c, d" does not match the type "int * int".


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}];
  run_ligo_bad ["print-ast-typed"; bad_contract "funarg_tuple_wrong.religo"];
  [%expect {|
    ligo: error
          in file "funarg_tuple_wrong.religo", line 1, characters 10-17
          The tuple "b, c, d" does not match the type "(int, int)".


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}];