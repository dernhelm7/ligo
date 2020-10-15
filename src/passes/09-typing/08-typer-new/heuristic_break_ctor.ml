(* selector / propagation rule for breaking down composite types
 * For now: break pair(a, b) = pair(c, d) into a = c, b = d *)

open Ast_typed.Misc
open Ast_typed.Types
open Typesystem.Solver_types
open Trace
open Typer_common.Errors

let selector :  (type_constraint_simpl, output_break_ctor, unit) selector =
  (* find two rules with the shape x = k(var …) and x = k'(var' …) *)
  fun type_constraint_simpl () dbs ->
  match type_constraint_simpl with
    SC_Constructor c ->
    (* finding other constraints related to the same type variable and
       with the same sort of constraint (constructor vs. constructor)
       is symmetric *)
    let other_cs = (Constraint_databases.get_constraints_related_to c.tv dbs).constructor in
    let other_cs = List.filter (fun (o : c_constructor_simpl) -> Var.equal c.tv o.tv) other_cs in
    let cs_pairs = List.map (fun x -> { a_k_var = c ; a_k'_var' = x }) other_cs in
    () , cs_pairs
  | SC_Alias       _                -> () , [] (* TODO: ??? (beware: symmetry) *)
  | SC_Poly        _                -> () , [] (* TODO: ??? (beware: symmetry) *)
  | SC_Typeclass   _                -> () , []
  | SC_Row         _                -> () , []

let propagator : (output_break_ctor , unit , typer_error) propagator =
  fun () dbs selected ->
  let () = ignore (dbs) in (* this propagator doesn't need to use the dbs *)
  let a = selected.a_k_var in
  let b = selected.a_k'_var' in

  (* The selector is expected to provice two constraints with the shape x = k(var …) and x = k'(var' …) *)
  assert (Var.equal (a : c_constructor_simpl).tv (b : c_constructor_simpl).tv);

  (* produce constraints: *)

  (* a.tv = b.tv *)
  let eq1 = c_equation { tsrc = "solver: propagator: break_ctor a" ; t = P_variable a.tv} { tsrc = "solver: propagator: break_ctor b" ; t = P_variable b.tv} "propagator: break_ctor" in
  let () = if Ast_typed.Debug.debug_new_typer then
           let p = Ast_typed.PP.c_constructor_simpl in
           Format.printf "\npropagator_break_ctor\na = %a\nb = %a\n%!" p a p b in
  (* a.c_tag = b.c_tag *)
  if (Solver_should_be_generated.compare_simple_c_constant a.c_tag b.c_tag) <> 0 then
    (* TODO : use error monad *)
    failwith (Format.asprintf "type error: incompatible types, not same ctor %a vs. %a (compare returns %d)"
                Solver_should_be_generated.debug_pp_c_constructor_simpl a
                Solver_should_be_generated.debug_pp_c_constructor_simpl b
                (Solver_should_be_generated.compare_simple_c_constant a.c_tag b.c_tag))
  else
    (* Produce constraint a.tv_list = b.tv_list *)
    let%bind eqs3 = List.map2 (fun aa bb -> c_equation { tsrc = "solver: propagator: break_ctor aa" ; t = P_variable aa} { tsrc = "solver: propagator: break_ctor bb" ; t = P_variable bb} "propagator: break_ctor") a.tv_list b.tv_list
        ~ok ~fail:(fun _ _ -> fail @@ different_constant_tag_number_of_arguments __LOC__ a.c_tag b.c_tag (List.length a.tv_list) (List.length b.tv_list)) in
    let eqs = eq1 :: eqs3 in
    ok (() , [
        {
          remove_constraints = [];
          add_constraints = eqs;
          justification = "no removal so no justification needed"
        }
      ])

let heuristic =
  Propagator_heuristic
    {
      selector ;
      propagator ;
      printer = Ast_typed.PP.output_break_ctor ; (* TODO: use an accessor that can get the printer for PP or PP_json alike *)
      comparator = Solver_should_be_generated.compare_output_break_ctor ;
      initial_private_storage = () ;
    }
