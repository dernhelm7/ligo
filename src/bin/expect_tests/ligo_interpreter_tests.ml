open Cli_expect

let test basename = "./" ^ basename
let pwd = Sys.getcwd ()
let () = Sys.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "test" ; test "interpret_test.mligo" ] ;
  [%expect{|
    Everything at the top-level was executed.
    - test_lambda_call exited with value ().
    - test_higher_order1 exited with value ().
    - test_higher_order2 exited with value ().
    - test_higher_order3 exited with value ().
    - test_higher_order4 exited with value ().
    - test_concats exited with value ().
    - test_record_concat exited with value ().
    - test_record_patch exited with value ().
    - test_record_lambda exited with value ().
    - test_variant_match exited with value ().
    - test_bool_match exited with value ().
    - test_list_match exited with value ().
    - test_tuple_proj exited with value ().
    - test_list_const exited with value ().
    - test_options_match_some exited with value ().
    - test_options_match_none exited with value ().
    - test_is_nat_yes exited with value ().
    - test_is_nat_no exited with value ().
    - test_abs_int exited with value ().
    - test_nat_int exited with value ().
    - test_map_list exited with value ().
    - test_fold_list exited with value ().
    - test_comparison_int exited with value ().
    - test_comparison_string exited with value ().
    - test_divs_int exited with value ().
    - test_divs_nat exited with value ().
    - test_var_neg exited with value ().
    - test_sizes exited with value ().
    - test_modi exited with value ().
    - test_fold_while exited with value ().
    - test_assertion_pass exited with value ().
    - test_map_finds exited with value ().
    - test_map_fold exited with value ().
    - test_map_map exited with value ().
    - test_map_mem exited with value ().
    - test_map_remove exited with value ().
    - test_map_update exited with value ().
    - test_set_add exited with value ().
    - test_set_mem exited with value ().
    - test_recursion_let_rec_in exited with value ().
    - test_top_level_recursion exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "test" ; test "interpret_test_log.mligo" ] ;
  [%expect{|
    {a = 1 ; b = 2n ; c = "aaa"}
    One (())
    Everything at the top-level was executed. |}]

let%expect_test _ =
  run_ligo_good [ "test" ; test "test_now.mligo" ] ;
  [%expect{|
    "storage at origination"
    "2000-01-01T10:10:10Z"
    "setting now at:"
    "storage after calling"
    "2010-01-01T10:10:11Z"
    Everything at the top-level was executed.
    - test_ts exited with value timestamp(946721410).
    - test exited with value true. |}]

let%expect_test _ =
  run_ligo_good [ "test" ; test "test_now_from_file.mligo" ] ;
  [%expect{|
    "storage at origination"
    "2000-01-01T10:10:10Z"
    "setting now at:"
    "storage after calling"
    "2010-01-01T10:10:11Z"
    Everything at the top-level was executed.
    - test_ts exited with value timestamp(946721410).
    - test exited with value true. |}]

let%expect_test _ =
  run_ligo_good [ "test" ; test "test_fail.mligo" ] ;
  [%expect {|
  Everything at the top-level was executed.
  - test exited with value "my contract always fail". |}]

let%expect_test _ =
  run_ligo_good [ "test" ; test "test_fail_from_file.mligo" ] ;
  [%expect {|
  Everything at the top-level was executed.
  - test exited with value "my contract always fail". |}]


let%expect_test _ =
  run_ligo_good [ "test" ; test "compile_expr.mligo" ] ;
  [%expect {|
  Everything at the top-level was executed.
  - test1 exited with value ().
  - test2 exited with value ().
  - test3 exited with value ().
  - test4 exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "test" ; test "compile_expr_from_file.mligo" ] ;
  [%expect {|
  Everything at the top-level was executed.
  - test1 exited with value ().
  - test2 exited with value ().
  - test3 exited with value ().
  - test4 exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "test" ; test "test_example.mligo" ] ;
  [%expect {|
  Everything at the top-level was executed.
  - test exited with value 111.
  - test2 exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "test" ; test "test_example_from_file.mligo" ] ;
  [%expect {|
  Everything at the top-level was executed.
  - test exited with value 111.
  - test2 exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "test" ; test "test_subst_with_storage.mligo" ] ;
  [%expect {|
  Everything at the top-level was executed.
  - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "test" ; test "test_subst_with_storage_from_file.mligo" ] ;
  [%expect {|
  Everything at the top-level was executed.
  - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "test" ; test "bootstrapped_contracts.mligo" ] ;
  [%expect {|
    "Initial states:"
    (Pair "KT1CSKPf2jeLpMmrgKquN2bCjBTkAcAdRVDy" 12)
    (Pair "KT1QuofAgnsWffHzLA7D78rxytJruGHDe7XG" 9)
    "Final states:"
    (Pair "KT1CSKPf2jeLpMmrgKquN2bCjBTkAcAdRVDy" 3)
    (Pair "KT1QuofAgnsWffHzLA7D78rxytJruGHDe7XG" 0)
    Everything at the top-level was executed.
    - test_transfer exited with value ().
            |}]

let%expect_test _ =
  run_ligo_good [ "test" ; test "override_function.mligo" ] ;
  [%expect {|
    4
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "test" ; test "test_fresh.mligo" ] ;
  [%expect {|
    Everything at the top-level was executed. |}]

(* do not remove that :) *)
let () = Sys.chdir pwd

let bad_test n = bad_test ("/interpreter_tests/"^n)

let%expect_test _ =
  run_ligo_bad [ "test" ; bad_test "test_failure1.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative//interpreter_tests/test_failure1.mligo", line 2, characters 2-25:
      1 | let test =
      2 |   failwith "I am failing"

    Test failed with "I am failing" |}]

let%expect_test _ =
  run_ligo_bad [ "test" ; bad_test "test_failure2.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative//interpreter_tests/test_failure2.mligo", line 2, characters 4-16:
      1 | let test =
      2 |     assert false

    Failed assertion |}]

let%expect_test _ =
  run_ligo_bad [ "test" ; bad_test "test_failure3.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative//interpreter_tests/test_failure3.mligo", line 2, characters 11-38:
      1 | let test =
      2 |   let ut = Test.reset_state 2n [1n;1n] in
      3 |   let f = (fun (_ : (unit * unit)) -> ()) in

    An uncaught error occured:
    Insufficient tokens in initial accounts to create one roll |}]

let%expect_test _ =
  run_ligo_bad [ "test" ; bad_test "test_trace.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 3, characters 5-24:
      2 |   if x < 0 then
      3 |     (failwith "negative" : int)
      4 |   else

    Test failed with "negative"
    Trace:
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 5, characters 4-13 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 5, characters 4-13 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 5, characters 4-13 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 5, characters 4-13 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 5, characters 4-13 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 9, characters 39-46 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 9, characters 14-49 |}]

let%expect_test _ =
  run_ligo_bad [ "test" ; bad_test "test_trace2.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative//interpreter_tests/test_trace2.mligo", line 6, characters 10-88:
      5 | let make_call (contr : unit contract) =
      6 |   let _ = Test.get_storage_of_address ("tz1fakefakefakefakefakefakefakcphLA5" : address) in
      7 |   Test.transfer_to_contract_exn contr () 10tez

    An uncaught error occured:
    Did not find service: GET ocaml:context/contracts/tz1fakefakefakefakefakefakefakcphLA5/storage
    Trace:
    File "../../test/contracts/negative//interpreter_tests/test_trace2.mligo", line 12, characters 2-33 |}]
