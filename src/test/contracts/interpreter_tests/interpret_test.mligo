let test_lambda_call =
  let a = 3 in
  let foo = fun (i : int) -> i * i in
  assert ((foo (a + 1)) = 16)

let test_higher_order1 =
  let a = 2 in
  let foo = fun (i : int) (j : int) (k : int) -> a + i + j + 0 in
  let bar = (foo 1 2) in
  assert (bar 3 = 5)

let test_higher_order2 =
  let a = 2 in
  let foo = fun (i : int) ->
              let b = 2 in
              let bar = fun (i : int) -> i + a + b
              in bar i
  in
  assert (foo 1 = 5)

let test_higher_order3 =
  let foo = fun (i : int) -> i + 1 in
  let bar = fun (f : int -> int) (i : int) -> f i + 1 in
  let baz : int -> int = bar foo
  in
  assert (baz 3 = 5)

let test_higher_order4 =
  let a = 3 in
  let foo = fun (i : int) -> a + i in
  let bar : int -> int = fun (i : int) -> foo i
  in
  assert (bar 2 = 5)

let test_concats = assert (0x70 ^ 0x70  = 0x7070)

type foo_record = {
  a : string;
  b : string
}

let test_record_concat =
  let ab : foo_record = {a="a"; b="b"}
  in assert (ab.a ^ ab.b = "ab")

let test_record_patch =
  let ab : foo_record = {a="a"; b="b"} in
  let res = {ab with b = "c"} in
  assert (res.b = "c")

type bar_record = {
  f   : int -> int;
  arg : int
}

let test_record_lambda =
  let a = 1 in
  let foo : int -> int = fun (i : int) -> a + i*2 in
  let farg : bar_record = {f = foo; arg = 2}
  in assert (farg.f farg.arg = 5)

type foo_variant =
| Foo
| Bar of int
| Baz of string

let test_variant_match =
  let a = Bar 1 in
  assert (match a with
  | Foo   -> false
  | Bar i -> true
  | Baz s -> false)

let test_bool_match =
  let b = true in
  assert (match b with
  | True -> true
  | False -> false)

let test_list_match =
  let a = [1; 2; 3; 4] in
  assert (match a with
  | hd::tl -> true
  | [] -> false)

let test_tuple_proj =
  let a, b = true, false
  in assert (a or b)

let test_list_const =
  let a = [1; 2; 3; 4] in
  assert ((List.length (0::a)) = 5n)

type foobar = int option

let test_options_match_some =
  let a = Some 0 in
  assert (match a with
  | Some i -> true
  | None -> false)

let test_options_match_none =
  let a : foobar = None in
  assert (match a with
  | Some i -> false
  | None -> true)

let test_is_nat_yes =
  let i : int = 1 in
  assert (match (is_nat i) with
  | Some i -> true
  | None -> false)

let test_is_nat_no =
  let j : int = -1 in
  assert (match (is_nat j) with
  | Some i -> false
  | None -> true)

let test_abs_int =
  let a : nat = abs (-5) in
  assert (a = 5n)

let test_nat_int = assert ((int 5n) = 5)

let test_map_list =
  let a = [1; 1; 1; 1] in
  let add_one : (int -> int) = fun (i : int) -> i + 1 in
  assert (match (List.map add_one a) with
  | hd::tl -> (hd = 2)
  | [] -> false)

let test_fold_list =
  let a = [1; 2; 3; 4] in
  let acc : int * int -> int =
    fun (prev, el : int * int) -> prev + el
  in
  assert ((List.fold acc a 0) = 10)

let test_comparison_int =
  let a = 1 > 2 in
  let b = 2 > 1 in
  let c = 1 >= 2 in
  let d = 2 >= 1 in
  assert ( not(a) && b && (not c) && d )

let test_comparison_string = assert (not("foo"="bar") && ("baz"="baz"))

let test_divs_int =
  let a = 1/2 in
  assert (a = 0)

let test_divs_nat =
  let a = 1n/2n in
  assert (a = 0n)

let test_var_neg =
  let a = 2 in
  assert (-a = -2)

let test_sizes =
  let a = [1; 2; 3; 4; 5] in
  let b = "12345" in
  let c = Set.literal [1; 2; 3; 4; 5] in
  let d = Map.literal [(1,1); (2,2); (3,3) ; (4,4) ; (5,5)] in
  let e = 0xFFFF in
  assert ((List.length a = 5n) &&
          (String.length b = 5n) &&
          (Set.cardinal c = 5n) &&
          (Map.size d = 5n) &&
          (Bytes.length e = 2n))

let test_modi = assert (3 mod 2 = 1n)

let test_fold_while =
  let aux : int -> bool * int =
    fun (i : int) ->
    if i < 10 then Loop.resume (i + 1) else Loop.stop i
  in
  assert ((Loop.fold_while aux 20 = 20) &&  (Loop.fold_while aux 0 = 10))

let test_assertion_pass =
  let unitt = assert (1=1) in
  assert true

let test_map_finds =
  let m = Map.literal [("one", 1); ("two", 2); ("three", 3)]
  in
  assert (match (Map.find_opt "two" m) with
  | Some v -> true
  | None -> false)

let m = Map.literal [("one", 1); ("two", 2); ("three", 3)]

let test_map_fold =
  let aux = fun (i : int * (string * int)) -> i.0 + i.1.1
  in assert (Map.fold aux m 0 = 6)

let test_map_map =
  let aux = fun (i : string * int) -> i.1 + String.length i.0 in
  assert (Map.find "one" (Map.map aux m) = 4)

let test_map_mem = assert ((Map.mem "one" m) && (Map.mem "two" m) && (Map.mem "three" m))

let test_map_remove =
  let m = Map.remove "one" m in
  let m = Map.remove "two" m in
  let m = Map.remove "three" m in
  assert (not (Map.mem "one" m) &&  not (Map.mem "two" m) && not (Map.mem "three" m))

let test_map_update =
  let m1 = Map.update "four" (Some 4) m in
  let m2 = Map.update "one" (None : int option) m in
  assert ((Map.mem "four" m1) && not (Map.mem "one" m2))

let s = Set.literal [1; 2; 3]

let test_set_add =
  let s = Set.add 1 (Set.empty : int set) in
  assert (Set.mem 1 s)

let test_set_mem =
  assert ((Set.mem 1 s) && (Set.mem 2 s) && (Set.mem 3 s))

let test_recursion_let_rec_in =
  let rec sum : int*int -> int = fun ((n,res):int*int) ->
    if (n<1) then res else sum (n-1,res+n)
  in
  assert (sum (10,0) = 55)

let rec sum_rec ((n,acc):int * int) : int =
    if (n < 1) then acc else sum_rec (n-1, acc+n)

let test_top_level_recursion = assert (sum_rec (10,0) = 55)
