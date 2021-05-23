open Cli_expect

let bad_test s = (bad_test "")^"vars_consts/"^s
let good_test s = (test "")^"vars_consts/"^s

(* Negatives *)

let%expect_test _ =
  run_ligo_bad [ "print-ast-core" ; (bad_test "match.ligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/vars_consts/match.ligo", line 9, characters 8-14:
      8 |     | Some (s) -> block {
      9 |         s := 3;
     10 |         result := s; }

    Cannot be assigned. |}]

let%expect_test _ =
  run_ligo_bad [ "print-ast-core" ; (bad_test "match.jsligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/vars_consts/match.jsligo", line 7, characters 23-24:
      6 |   let store2 = match (action, {
      7 |     Add: (n: int) => { n = 42; return n; },
      8 |     Sub: (n: int) => { n = 42; return -n; }

    Cannot be assigned. |}]

let%expect_test _ =
  run_ligo_bad [ "print-ast-core" ; (bad_test "assign_const_param.ligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/vars_consts/assign_const_param.ligo", line 3, characters 4-10:
      2 |   block {
      3 |     x := 4;
      4 |   } with x

    Cannot be assigned. |}]

let%expect_test _ =
  run_ligo_bad [ "print-ast-core" ; (bad_test "assign_consts.ligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/vars_consts/assign_consts.ligo", line 4, characters 4-10:
      3 |     const (x, y) = (4, 5);
      4 |     x := 1;
      5 |   } with x + y + z

    Cannot be assigned. |}]

let%expect_test _ =
  run_ligo_bad [ "print-ast-core" ; (bad_test "assign_consts.jsligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/vars_consts/assign_consts.jsligo", line 3, characters 2-3:
      2 |   const [x, y] = [4, 5];
      3 |   x = 1;
      4 |   return (x + y + z);

    Cannot be assigned. |}]

let%expect_test _ =
  run_ligo_bad [ "print-ast-core" ; (bad_test "assign_const_params.ligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/vars_consts/assign_const_params.ligo", line 3, characters 4-10:
      2 |   block {
      3 |     x := 4;
      4 |     y := 3;

    Cannot be assigned. |}]

let%expect_test _ =
  run_ligo_bad [ "print-ast-core" ; (bad_test "capture_var_param.ligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/vars_consts/capture_var_param.ligo", line 1, characters 17-18:
      1 | function foo(var x : int) : int is
      2 |   block {

    Cannot be captured. |}]

let%expect_test _ =
  run_ligo_bad [ "print-ast-core" ; (bad_test "capture_var_params.ligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/vars_consts/capture_var_params.ligo", line 1, characters 17-18:
      1 | function foo(var x : int; const y : int) : int -> int is
      2 |   block {

    Cannot be captured. |}]

let%expect_test _ =
  run_ligo_bad [ "print-ast-core" ; (bad_test "capture_var_params.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/vars_consts/capture_var_params.mligo", line 4, characters 6-7:
      3 |     match p with
      4 |       x[@var], y ->
      5 |         let bar : unit -> int = fun (_ : unit) -> x + y in

    Cannot be captured. |}]

let%expect_test _ =
  run_ligo_bad [ "print-ast-core" ; (bad_test "assign_const_param.jsligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/vars_consts/assign_const_param.jsligo", line 5, characters 5-8:
      4 |      const age: int = 3; // does not give an error
      5 |      age = 42; // does give an error
      6 |      return age;

    Cannot be assigned. |}]

let%expect_test _ =
  run_ligo_bad [ "print-ast-core" ; (bad_test "assign_const_param_2.jsligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/vars_consts/assign_const_param_2.jsligo", line 2, characters 2-3:
      1 | let x = (a: int): int => {
      2 |   a = 42;
      3 |   return a;

    Cannot be assigned. |}]

let%expect_test _ =
  run_ligo_bad [ "print-ast-core" ; (bad_test "multiple_vars_1.jsligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/vars_consts/multiple_vars_1.jsligo", line 4, characters 4-5:
      3 |     const [x,y] = [4,5];
      4 |     x = 2;
      5 |     y = 3;

    Cannot be assigned. |}]

let%expect_test _ =
  run_ligo_bad [ "print-ast-core" ; (bad_test "multiple_vars_2.jsligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/vars_consts/multiple_vars_2.jsligo", line 3, characters 11-12:
      2 |   {
      3 |     let [x,y] = [4,5];
      4 |     let add = (_ : unit) : int => { return (x + y); };
     ,
    File "../../test/contracts/negative/vars_consts/multiple_vars_2.jsligo", line 3, characters 9-10:
      2 |   {
      3 |     let [x,y] = [4,5];
      4 |     let add = (_ : unit) : int => { return (x + y); };

    Cannot be captured. |}]

let%expect_test _ =
  run_ligo_bad [ "print-ast-core" ; (bad_test "multiple_vars_1.ligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/vars_consts/multiple_vars_1.ligo", line 4, characters 4-10:
      3 |     const (x, y) = (4, 5);
      4 |     x := 2;
      5 |     y := 3;

    Cannot be assigned. |}]

let%expect_test _ =
  run_ligo_bad [ "print-ast-core" ; (bad_test "multiple_vars_2.ligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/vars_consts/multiple_vars_2.ligo", line 3, characters 12-13:
      2 |   block {
      3 |     var (x, y) := (4, 5);
      4 |     function add(const _u : unit) is (x + y);
     ,
    File "../../test/contracts/negative/vars_consts/multiple_vars_2.ligo", line 3, characters 9-10:
      2 |   block {
      3 |     var (x, y) := (4, 5);
      4 |     function add(const _u : unit) is (x + y);

    Cannot be captured. |}]

(* Positives *)

let%expect_test _ =
  run_ligo_good [ "print-ast-core" ; (good_test "shadowing.ligo") ] ;
  [%expect{|
    const foo =
      lambda (toto : int) return let toto[@var] = 2 in let toto = 3 in toto ,
    const bar =
      lambda (_u[@var] : unit) return let toto = 1 in
                                      let toto[@var] = 2 in let toto = 3 in toto |}]
let%expect_test _ =
  run_ligo_good [ "print-ast-core" ; (good_test "shadowing.jsligo") ] ;
  [%expect{|
    const foo[@var] =
      rec (foo:int -> int => lambda (toto : int) : int return let toto[@var] =
                                                                2 in
                                                              let toto =
                                                              3 in toto ) ,
    const bar[@var] =
      rec (bar:int -> int => lambda (toto : int) : int return let toto =
                                                              1 in
                                                              let toto[@var] =
                                                                2 in
                                                              let toto =
                                                              3 in toto ) |}]

let%expect_test _ =
  run_ligo_good [ "print-ast-core" ; (good_test "func_const_var.ligo") ] ;
  [%expect{|
    const foo : int -> int -> int =
      lambda (x : int) : int -> int return let bar : int -> int =
                                             lambda (y[@var] : int) : int return
                                             ADD(x , y) in
                                           bar |}]

let%expect_test _ =
  run_ligo_good [ "print-ast-core" ; (good_test "func_same_const_var.ligo") ] ;
  [%expect{|
    const foo : int -> int =
      lambda (x : int) : int return let bar : int -> int =
                                      lambda (x[@var] : int) : int return
                                      let x = ADD(x , 1) in x in
                                    (bar)@(42) |}]

let%expect_test _ =
  run_ligo_good [ "print-ast-core" ; (good_test "func_var_const.ligo") ] ;
  [%expect{|
    const foo : int -> int =
      lambda (x[@var] : int) : int return let bar : int -> int =
                                            lambda (x : int) : int return x in
                                          (bar)@(42) |}]

let%expect_test _ =
  run_ligo_good [ "print-ast-core" ; (good_test "var_loop.ligo") ] ;
  [%expect{|
    const foo : int -> int =
      lambda (x : int) : int return let i[@var] = 0 in
                                    let b[@var] = 5 in
                                    let env_rec#1 = ( record[i -> i] ) in
                                    let env_rec#1 =
                                      FOLD_WHILE(lambda (binder#2) return
                                                 let i = binder#2.0.i in
                                                  match AND(LT(i , x) ,
                                                            GT(b , 0)) with
                                                   | true () -> CONTINUE(let i =
                                                                        ADD
                                                                        (i ,
                                                                        1) in
                                                                        let binder#2 =
                                                                        {
                                                                        binder#2
                                                                        with
                                                                        { 0 =
                                                                        {
                                                                        binder#2.0
                                                                        with
                                                                        { i =
                                                                        i } } } } in
                                                                        let _ : unit =
                                                                        unit in
                                                                        binder#2)
                                                   | false () -> STOP(binder#2) ,
                                                 env_rec#1) in
                                    let env_rec#1 = env_rec#1.0 in
                                    let i = env_rec#1.i in i |}]

let%expect_test _ =
  run_ligo_good [ "print-ast" ; (good_test "multiple_vars.ligo") ] ;
  [%expect{|
    const foo =
      lambda (_u : unit) return  match (4 , 5) with
                                  | (x[@var],y[@var]) -> { x := 2;
     { y := 3;
     ADD(x , y)}}
    const bar =
      lambda (_u : unit) return  match (4 , 5) with
                                  | (x,y) -> let add =
                                               lambda (_u : unit) return ADD(x ,
                                             y) in (add)@(UNIT()) |}]

let%expect_test _ =
  run_ligo_good [ "print-ast" ; (good_test "multiple_vars.jsligo") ] ;
  [%expect{|
    const foo[@var] =
      rec (foo:unit -> int => lambda (_ : unit) : int return  match (4 , 5) with
                                                               | (x[@var],y[@var]) -> { x := 2;
     { y := 3;
     C_POLYMORPHIC_ADD(x , y)}} )
    const bar[@var] =
      rec (bar:unit -> int => lambda (_ : unit) : int return  match (4 , 5) with
                                                               | (x,y) ->
                                                               let add[@var] =
                                                                 rec (add:unit -> int => lambda (_ : unit) : int return C_POLYMORPHIC_ADD(x ,
                                                               y) ) in
                                                               (add)@(unit) ) |}]
