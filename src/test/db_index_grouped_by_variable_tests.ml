open Trace
open Ast_typed.Types
open Database_plugins.All_plugins
open Db_index_tests_common

open GroupedByVariable

let merge_in_state ~demoted_repr ~new_repr state =
  let updater = {
    map = (fun m -> UnionFind.ReprMap.alias ~demoted_repr ~new_repr m);
    set = (fun s -> UnionFind.ReprSet.alias ~demoted_repr ~new_repr s);
  } in
  merge_aliases updater state

let merge_in_repr ~demoted_repr ~new_repr repr =
  fun tv -> match repr tv with
      tv when Var.equal tv demoted_repr -> new_repr
    | other -> other

let merge ~demoted_repr ~new_repr repr state =
  if (not (Var.equal (repr demoted_repr) demoted_repr)) ||
     (not (Var.equal (repr new_repr) new_repr))
  then
    failwith "Internal error: bad test: the demoted_repr and new_repr \
              should already be representants when merge is called."
  else
    ((merge_in_repr ~demoted_repr ~new_repr repr),
     (merge_in_state ~demoted_repr ~new_repr state))

(* can't be defined easily in MultiSet.ml because it doesn't have access to List.compare ~cmp  *)
let multiset_compare a b =
  let ab = List.compare ~compare:(MultiSet.get_compare a) (MultiSet.elements a) (MultiSet.elements b) in
  let ba = List.compare ~compare:(MultiSet.get_compare b) (MultiSet.elements a) (MultiSet.elements b) in
  if ab != ba
  then failwith "Internal error: bad test: sets being compared have different comparison functions!"
  else ab

module Grouped_by_variable_tests = struct
  include Test_vars
  module Plugin_under_test = GroupedByVariable
  include Plugin_under_test
  let repr : type_variable -> type_variable = fun tv ->
    match tv with
    | tv when Var.equal tv tva -> tva
    | tv when Var.equal tv tvb -> tva
    | _ -> tv

  let cmp x y =
    List.compare ~compare:(Pair.compare Var.compare multiset_compare)
      (List.filter (fun (_,s) -> not (MultiSet.is_empty s)) x)
      (List.filter (fun (_,s) -> not (MultiSet.is_empty s)) y)
  let same_state' loc (expected : _ t_for_tests) (actual : _ t_for_tests) =
    let expected_actual_str =
      let open PP_helpers in
      let pp' pp x = (list_sep_d (pair Var.pp (MultiSet.pp pp))) x in
      Format.asprintf "expected=\n{ctors=\n%a;\nrows=\n%a;\npolys=\n%a}\nactual=\n{ctors=\n%a;\nrows=\n%a;\npolys=\n%a}"
        (pp' Ast_typed.PP.c_constructor_simpl) expected.constructor
        (pp' Ast_typed.PP.c_row_simpl        ) expected.row
        (pp' Ast_typed.PP.c_poly_simpl       ) expected.poly
        (pp' Ast_typed.PP.c_constructor_simpl) actual.constructor
        (pp' Ast_typed.PP.c_row_simpl        ) actual.row
        (pp' Ast_typed.PP.c_poly_simpl       ) actual.poly
    in
    let a msg expected actual =
      tst_assert (Format.asprintf "%s\n%s\n%s\n" msg loc expected_actual_str)
        (cmp expected actual = 0)
    in
    let%bind () = a "lists of ctors must be equal" expected.constructor actual.constructor in
    let%bind () = a "lists of rows must be equal"  expected.row actual.row in
    let%bind () = a "lists of polys must be equal" expected.poly actual.poly in
    ok ()

  let same_state (expected : _ t) (actual : _ t) =
    same_state' __LOC__ (GroupedByVariable.bindings expected) (GroupedByVariable.bindings actual)
end

open Grouped_by_variable_tests

type nonrec t_for_tests = type_variable GroupedByVariable.t_for_tests

let filter_only_ctors  = List.filter_map (function Ast_typed.Types.SC_Constructor c -> Some c | _ -> None)
let filter_only_rows   = List.filter_map (function Ast_typed.Types.SC_Row         c -> Some c | _ -> None)
let filter_only_polys  = List.filter_map (function Ast_typed.Types.SC_Poly        c -> Some c | _ -> None)
let to_ctor_sets = List.map (fun (v,cs) -> (v, (MultiSet.of_list ~cmp:Ast_typed.Compare.c_constructor_simpl (filter_only_ctors cs))))
let to_row_sets  = List.map (fun (v,cs) -> (v, (MultiSet.of_list ~cmp:Ast_typed.Compare.c_row_simpl         (filter_only_rows  cs))))
let to_poly_sets = List.map (fun (v,cs) -> (v, (MultiSet.of_list ~cmp:Ast_typed.Compare.c_poly_simpl        (filter_only_polys cs))))

let assert_states_equal
    loc
    ?(expected_ctors:(type_variable * type_constraint_simpl list) list = [])
    ?(expected_rows:(type_variable * type_constraint_simpl list) list = [])
    ?(expected_polys:(type_variable * type_constraint_simpl list) list = [])
    (actual:type_variable t) =
  same_state'
    loc
    {
      constructor = to_ctor_sets expected_ctors ;
      row         = to_row_sets  expected_rows  ;
      poly        = to_poly_sets expected_polys ;
    }
    (GroupedByVariable.bindings actual)

let remove_constraint repr state constraint_to_rm =
  trace Main_errors.typer_tracer @@
  remove_constraint repr state constraint_to_rm

let first_test () =
  let repr : type_variable -> type_variable = fun tv -> tv in
  let state = create_state ~cmp:Ast_typed.Compare.type_variable in

  let repr = merge_in_repr ~demoted_repr:tvb ~new_repr:tva repr in

  (* create constraints and add them to the state *)
  let sc_a : type_constraint_simpl = constructor 1 None tva C_unit [] in
  let sc_b : type_constraint_simpl = constructor 2 None tvb C_unit [] in
  let sc_c : type_constraint_simpl = constructor 3 None tvc C_unit [] in
  let state = add_constraint repr state sc_a in
  let state = add_constraint repr state sc_b in
  let state = add_constraint repr state sc_c in
  (* 
    check that :
    - a is associated with sc_a and sc_b
    - c is associated wit sc_c
    - b has no associated constraint (because repr(b) = a)
  *)
  let%bind () = assert_states_equal __LOC__
    ~expected_ctors:[(tva, [sc_a ; sc_b]) ; (tvc, [sc_c])]
    state in

  ok ()

let second_test () =
  let repr : type_variable -> type_variable = fun tv -> tv in
  let state = create_state ~cmp:Ast_typed.Compare.type_variable in

  let repr = merge_in_repr ~demoted_repr:tvb ~new_repr:tva repr in

  (* create constraints and add them to the state *)
  let sc_a : type_constraint_simpl = constructor 1 None tva C_unit [] in
  let sc_b : type_constraint_simpl = constructor 2 None tvb C_unit [] in
  let sc_c : type_constraint_simpl = constructor 3 None tvc C_unit [] in
  let state = add_constraint repr state sc_a in
  let state = add_constraint repr state sc_b in
  let state = add_constraint repr state sc_c in
  (* 
    check that :
    - a is associated with sc_a and sc_b
    - c is associated wit sc_c
    - b has no associated constraint (because repr(b) = a)
  *)
  let%bind () = assert_states_equal __LOC__
      ~expected_ctors:[(tva, [sc_a ; sc_b]) ; (tvc, [sc_c])]
      state in

  (* remove sc_a from state *)
  let%bind state = remove_constraint repr state sc_a in
  (* same check as above except sc_a should be deleted from tva's constraints *)
  let%bind () = assert_states_equal __LOC__
      ~expected_ctors:[(tva, [sc_b]) ; (tvc, [sc_c])]
      state in

  (* merge variable c into a *)
  let repr, state = merge ~demoted_repr:tvc ~new_repr:tva repr state in
  (* same check as above except sc_c should now be in a's constraints *)
  let%bind () = assert_states_equal __LOC__
      ~expected_ctors:[(tva, [sc_b; sc_c])]
      state in

  (* create constraint and add it to the state *)
  let sc_d : type_constraint_simpl = constructor 4 None tvd C_unit [] in
  let state = add_constraint repr state sc_d in
  (* same check as above except sc_d should be added to d's constraints (was empty / absent before) *)
  let%bind () = assert_states_equal __LOC__
      ~expected_ctors:[(tva, [sc_b; sc_c]) ; (tvd, [sc_d])]
      state in

  (* create constraint and add it to the state *)
  let sc_a2 : type_constraint_simpl = constructor 5 None tva C_unit [] in
  let state = add_constraint repr state sc_a2 in
  (* same check as above except sc_d should be added to a's constraints *)
  let%bind () = assert_states_equal __LOC__
      ~expected_ctors:[(tva, [sc_a2; sc_b; sc_c]) ; (tvd, [sc_d])]
      state in

  (* create constraint and add it to the state *)
  let sc_b2 : type_constraint_simpl = constructor 6 None tvb C_unit [] in
  let state = add_constraint repr state sc_b2 in
  (* same check as above except sc_d should be added to a's constraints *)
  let%bind () = assert_states_equal __LOC__
      ~expected_ctors:[(tva, [sc_a2; sc_b; sc_b2; sc_c]) ; (tvd, [sc_d])]
      state in

  ok ()

let add_and_merge ~sc_a ~sc_b ~sc_c check =
  let repr : type_variable -> type_variable = fun tv -> tv in
  let state = create_state ~cmp:Ast_typed.Compare.type_variable in

  (* Add contraint sc_a *)
  let state = add_constraint repr state sc_a in

  (* Test one; state is { a -> [sc_a]} *)
  let%bind () = check __LOC__ [(tva, [sc_a])] state in

  (* Add constraint sc_b *)
  let state = add_constraint repr state sc_b in

  (* Test two; state is { a -> [sc_a]; b -> [sc_b]} *)
  let%bind () = check __LOC__ [(tva, [sc_a]); (tvb, [sc_b])] state in

  (* merge variable b into a *)
  let repr, state = merge ~demoted_repr:tvb ~new_repr:tva repr state in
  
  (* Test three; state is { a -> [sc_a;sc_b]} *)
  (* same check as above except sc_b should now be in a's constraints *)
  let%bind () = check __LOC__ [(tva, [sc_a; sc_b])] state in

  (* Add constraint sc_c *)
  let state = add_constraint repr state sc_c in

  (* Test four; state is { a -> [sc_a;sc_b]; c -> [sc_c]} *)
  let%bind () = check __LOC__ [(tva, [sc_a; sc_b]) ; (tvc, [sc_c])] state in

  ok ()

(* test add ctor constraint + add other ctor constraint + merge + add third ctor constraint *)
let ctor_add_and_merge () =
  let sc_a : type_constraint_simpl = constructor 1 None tva C_unit [] in
  let sc_b : type_constraint_simpl = constructor 2 None tvb C_unit [] in
  let sc_c : type_constraint_simpl = constructor 3 None tvc C_unit [] in
  add_and_merge ~sc_a ~sc_b ~sc_c (fun loc expected_ctors state -> assert_states_equal (loc ^ "\n" ^ __LOC__) ~expected_ctors state)

(* test add row constraint + add other row constraint + merge + add third row constraint *)
let row_add_and_merge () =
  let sc_a : type_constraint_simpl = row 1 tva in
  let sc_b : type_constraint_simpl = row 2 tvb in
  let sc_c : type_constraint_simpl = row 3 tvc in
  add_and_merge ~sc_a ~sc_b ~sc_c (fun loc expected_rows state -> assert_states_equal (loc ^ "\n" ^ __LOC__) ~expected_rows state)


(* test add poly constraint + add other poly constraint + merge poly constraint + add third poly constraint *)
let poly_add_and_merge () =
  let p_forall : p_forall = {
    binder = Var.of_name "binder";
    constraints = [];
    body = Location.wrap @@ P_variable (Var.of_name "binder");
  } in
  let sc_a : type_constraint_simpl = poly tva p_forall in
  let sc_b : type_constraint_simpl = poly tvb p_forall in
  let sc_c : type_constraint_simpl = poly tvc p_forall in
  add_and_merge ~sc_a ~sc_b ~sc_c (fun loc expected_polys state -> assert_states_equal (loc ^ "\n" ^ __LOC__) ~expected_polys state)



let add_and_remove ~sc_a ~sc_b ~sc_c check =
  let repr : type_variable -> type_variable = fun tv -> tv in
  let state = create_state ~cmp:Ast_typed.Compare.type_variable in

  (* Add contraint sc_a *)
  let state = add_constraint repr state sc_a in

  (* Test one; state is { a -> [sc_a]} *)
  let%bind () = check __LOC__ [(tva, [sc_a])] state in
  
  (* Add constraint sc_b *)
  let state = add_constraint repr state sc_b in

  (* Test two; state is { a -> [sc_a]; b -> [sc_b]} *)
  let%bind () = check __LOC__ [(tva, [sc_a]); (tvb, [sc_b])] state in

  (* Remove constaint sc_b *)
  let%bind state = remove_constraint repr state sc_b in
  (* Test three; state is { a -> [sc_a]} *)
  let%bind () = check __LOC__ [(tva, [sc_a])] state in

  (* Add constraint sc_c *)
  let state = add_constraint repr state sc_c in

  (* Test four; state is { a -> [sc_a]; c -> [sc_b]} *)
  let%bind () = check __LOC__ [(tva, [sc_a]) ; (tvc, [sc_c])] state in

  ok ()

(*
    test add ctor constraint + add other ctor constraint + remove ctor constraint + add third ctor constraint
*)
let ctor_add_and_remove () =
  let sc_a : type_constraint_simpl = constructor 1 None tva C_unit [] in
  let sc_b : type_constraint_simpl = constructor 2 None tvb C_unit [] in
  let sc_c : type_constraint_simpl = constructor 3 None tvc C_unit [] in
  add_and_remove ~sc_a ~sc_b ~sc_c (fun loc expected_ctors state -> assert_states_equal (loc ^ "\n" ^ __LOC__) ~expected_ctors state)

(*
   test add row constraint + add other row constraint + remove row constraint + add third row constraint
*)
let row_add_and_remove () =
  let sc_a : type_constraint_simpl = row 1 tva in
  let sc_b : type_constraint_simpl = row 2 tvb in
  let sc_c : type_constraint_simpl = row 3 tvc in
  add_and_remove ~sc_a ~sc_b ~sc_c (fun loc expected_rows state -> assert_states_equal (loc ^ "\n" ^ __LOC__) ~expected_rows state)


(*
   test add poly constraint + add other poly constraint + remove poly constraint + add third poly constraint
*)
let poly_add_and_remove () =
  let p_forall : p_forall = {
    binder = Var.of_name "binder";
    constraints = [];
    body = Location.wrap @@ P_variable (Var.of_name "binder");
  } in
  let sc_a : type_constraint_simpl = poly tva p_forall in
  let sc_b : type_constraint_simpl = poly tvb p_forall in
  let sc_c : type_constraint_simpl = poly tvc p_forall in
  add_and_remove ~sc_a ~sc_b ~sc_c (fun loc expected_polys state -> assert_states_equal (loc ^ "\n" ^ __LOC__) ~expected_polys state)

(* Test mixed + remove + merge *)

let mixed () =
  let repr : type_variable -> type_variable = fun tv -> tv in
  let state = create_state ~cmp:Ast_typed.Compare.type_variable in

  (* add ctor *)
  let sc_a : type_constraint_simpl = constructor 10 None tva C_unit [] in
  let state = add_constraint repr state sc_a in

  (* Test 1: state is { a -> [sc_a]} *)
  let%bind () = assert_states_equal __LOC__
      ~expected_ctors:[(tva, [sc_a])]
      state in
  
  (* Add row *)
  let sc_b : type_constraint_simpl = row 11 tvb in
  let state = add_constraint repr state sc_b in
  (* Test 2: state is { a -> [sc_a]; b -> [sc_b]} *)

  (* Test 2; state is ctors = {a -> [sc_a]} rows = {b -> [sc_b]} *)
  let%bind () = assert_states_equal __LOC__
      ~expected_ctors:[(tva, [sc_a])]
      ~expected_rows:[(tvb, [sc_b])]
      state in

  (* Add poly*)
  let p_forall : p_forall = {
    binder = Var.of_name "binder";
    constraints = [];
    body = Location.wrap @@ P_variable (Var.of_name "binder");
  } in
  let sc_c : type_constraint_simpl = poly tvc p_forall in
  let state = add_constraint repr state sc_c in

  (* Test 3; state is ctors = {a -> [sc_a]} rows = {b -> [sc_b]} polys = {c -> [sc_c]} *)
  let%bind () = assert_states_equal __LOC__
      ~expected_ctors:[(tva, [sc_a])]
      ~expected_rows:[(tvb, [sc_b])]
      ~expected_polys:[(tvc, [sc_c])]
      state in

  (* Add constraint sc_c2 *)
  let sc_c2 = constructor 12 None tvc C_int [] in
  let state = add_constraint repr state sc_c2 in
  (* Test 4; state is ctors = {a -> [sc_a]; c -> [sc_c2]} rows = {b -> [sc_b]} polys = {c -> [sc_c]} *)
  let%bind () = assert_states_equal __LOC__
      ~expected_ctors:[(tva, [sc_a]) ; (tvc, [sc_c2])]
      ~expected_rows:[(tvb, [sc_b])]
      ~expected_polys:[(tvc, [sc_c])]
      state in
  
  (* merge variable b into a *)
  let repr, state = merge ~demoted_repr:tvb ~new_repr:tva repr state in

  (* Test 5; state is ctors = {a -> [sc_a]; c -> [sc_c2]} rows = {a -> [sc_b]} polys = {c -> [sc_c]} *)
  let%bind () = assert_states_equal __LOC__
      ~expected_ctors:[(tva, [sc_a]); (tvc, [sc_c2])]
      ~expected_rows:[(tva, [sc_b])]
      ~expected_polys:[(tvc, [sc_c])]
      state in

  (* Add constraint sc_b2 *)
  let sc_b2 = row ~row:[(Label "foo", tva)] 13 tvb in
  let state = add_constraint repr state sc_b2 in

  (* Test 6; state is ctors = {a -> [sc_a]; c -> [sc_c2]} rows = {a -> [sc_b; sc_b2]} polys = {c -> [sc_c]} *)
  let%bind () = assert_states_equal __LOC__
      ~expected_ctors:[(tva, [sc_a]); (tvc, [sc_c2])]
      ~expected_rows:[(tva, [sc_b; sc_b2])]
      ~expected_polys:[(tvc, [sc_c])]
      state in

  (* Remove constaint sc_b *)
  let%bind state = remove_constraint repr state sc_b in

  (* Test 7; state is ctors = {a -> [sc_a]; c -> [sc_c2]} rows = {a -> [sc_b2]} polys = {c -> [sc_c]} *)
  let%bind () = assert_states_equal __LOC__
      ~expected_ctors:[(tva, [sc_a]); (tvc, [sc_c2])]
      ~expected_rows:[(tva, [sc_b2])]
      ~expected_polys:[(tvc, [sc_c])]
      state in

  (* Add constraint sc_b3 *)
  let sc_b3 = row ~row:[(Label "foo", tva)] 13 tvb in
  let state = add_constraint repr state sc_b3 in

  (* Test 8; state is ctors = {a -> [sc_a]; c -> [sc_c2]} rows = {a -> [sc_b2; sc_b3]} polys = {c -> [sc_c]} *)
  let%bind () = assert_states_equal __LOC__
      ~expected_ctors:[(tva, [sc_a]); (tvc, [sc_c2])]
      ~expected_rows:[(tva, [sc_b2; sc_b3])]
      ~expected_polys:[(tvc, [sc_c])]
      state in

  (* Remove constaint sc_b2 *)
  let%bind state = remove_constraint repr state sc_b2 in

  (* Test 9; state is ctors = {a -> [sc_a]; c -> [sc_c2]} rows = {a -> [sc_b3]} polys = {c -> [sc_c]} *)
  let%bind () = assert_states_equal __LOC__
      ~expected_ctors:[(tva, [sc_a]); (tvc, [sc_c2])]
      ~expected_rows:[(tva, [sc_b3])]
      ~expected_polys:[(tvc, [sc_c])]
      state in

  ok ()

let grouped_by_variable () =
  let%bind () = first_test () in
  let%bind () = second_test () in
  let%bind () = ctor_add_and_merge () in
  let%bind () = ctor_add_and_remove () in
  let%bind () = row_add_and_merge () in
  let%bind () = row_add_and_remove () in
  let%bind () = poly_add_and_merge () in
  let%bind () = poly_add_and_remove () in
  let%bind () = mixed () in
  ok ()
