open Cli_expect

let contract basename =
  "../../test/contracts/" ^ basename

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "michelson_pair_tree.ligo" ; "main" ] ;
  [%expect {|
    Warning: unused variable #2 at
    Warning: unused variable #3 at
    { parameter unit ;
      storage (pair (string %three) (pair %four (int %one) (nat %two))) ;
      code { DROP ;
             PUSH nat 2 ;
             PUSH int 1 ;
             PAIR ;
             PUSH string "foo" ;
             PAIR ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "michelson_pair_tree.mligo" ; "main" ] ;
  [%expect {|
    Warning: unused variable #2 at
    Warning: unused variable #3 at
    { parameter unit ;
      storage (pair (int %three) (pair %four (int %one) (nat %two))) ;
      code { DROP ;
             PUSH nat 2 ;
             PUSH int 1 ;
             PAIR ;
             PUSH int 3 ;
             PAIR ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "michelson_pair_tree.religo" ; "main" ] ;
  [%expect {|
    Warning: unused variable #2 at
    Warning: unused variable #3 at
    { parameter unit ;
      storage (pair (int %three) (pair %four (int %one) (nat %two))) ;
      code { DROP ;
             PUSH nat 2 ;
             PUSH int 1 ;
             PAIR ;
             PUSH int 3 ;
             PAIR ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "michelson_pair_tree_intermediary.ligo" ; "main" ] ;
  [%expect {|
    Warning: unused variable #2 at
    Warning: unused variable #3 at
    { parameter unit ;
      storage (pair (string %three) (pair (int %one) (nat %two))) ;
      code { DROP ;
             PUSH nat 2 ;
             PUSH int 1 ;
             PAIR ;
             PUSH string "foo" ;
             PAIR ;
             NIL operation ;
             PAIR } } |}]