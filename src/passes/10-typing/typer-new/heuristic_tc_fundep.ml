(* selector / propagation rule for restricting a type class
     (α₁, α₂, …) ∈ { (τ₁₁, τ₁₂, …) , … }
   to the possible cases, given a second hypothesis of the form
     αᵢ = κ(β₁, β₂, …)
      or
     αᵢ = ρ(ℓᵢ : βᵢ, …)
   It restricts the number of possible cases and replaces αᵢ in
   tuple of constrained variables so that the βⱼ are constrained
   instead.

   This rule can deduce a new assignment for other variables
   constrained by the typeclass if every possible type for that
   variable uses the same type constructor. *)

(* TODO: have a heuristic that restricts typeclass constraints with
   repeated or aliased type variables in the arguments, i.e. of the
   form […;x;…;y;…] ∈ […] where x and y are identical or aliased. *)

open Trace
open Typer_common.Errors
open Simple_utils

module TYPE_VARIABLE_ABSTRACTION = Type_variable_abstraction.TYPE_VARIABLE_ABSTRACTION

module M = functor (Type_variable : sig type t end) (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) -> struct
  open Type_variable_abstraction
  open Type_variable_abstraction.Types
  open Type_variable_abstraction.Reasons

  module Utils = Heuristic_tc_fundep_utils.Utils(Type_variable)(Type_variable_abstraction)
  open Utils
  open Utils.All_plugins

  type selector_output = {
    tc : c_typeclass_simpl ;
    c :  constructor_or_row ;
  }

  let heuristic_name = "tc_fundep"

(* ***********************************************************************
 * Selector
 * *********************************************************************** *)

(* Find typeclass constraints in the dbs which constrain c.tv *)
let selector_by_ctor : (type_variable -> type_variable) -> flds -> c_constructor_simpl -> selector_output list =
  fun repr (module Indexes) c ->
  let typeclasses = (Typeclasses_constraining.get_typeclasses_constraining_list (repr c.tv) Indexes.typeclasses_constraining) in
  let cs_pairs_db = List.map (fun tc -> { tc ; c = `Constructor c }) typeclasses in
  cs_pairs_db

let selector_by_row : (type_variable -> type_variable) -> flds -> c_row_simpl -> selector_output list =
  fun repr (module Indexes) r ->
  let typeclasses = (Typeclasses_constraining.get_typeclasses_constraining_list (repr r.tv) Indexes.typeclasses_constraining) in
  let cs_pairs_db = List.map (fun tc -> { tc ; c = `Row r }) typeclasses in
  cs_pairs_db

(* Find constructor constraints α = κ(β …) where α is one of the
   variables constrained by the (refined version of the) typeclass
   constraint tcs. *)
let selector_by_tc : (type_variable -> type_variable) -> flds -> c_typeclass_simpl -> selector_output list =
  fun repr (module Indexes) tc ->
  let aux tv =
    (* Find the constructor constraints which apply to tv. *)
    (* Since we are only refining the typeclass one type expression
       node at a time, we only need the top-level assignment for
       that variable, e.g. α = κ(βᵢ, …). We can therefore look
       directly in the assignments. *)
    match Assignments.find_opt (repr tv) Indexes.assignments with
    | Some cr -> [({ tc ; c = cr } : selector_output)]
    | None   -> [] in
  List.flatten @@ List.map aux tc.args

let selector : (type_variable -> type_variable) -> type_constraint_simpl -> flds -> selector_output list =
  fun repr type_constraint_simpl indexes ->
  match type_constraint_simpl with
    SC_Constructor c  -> selector_by_ctor repr indexes c
  | SC_Row r          -> selector_by_row repr indexes r
  | SC_Alias        _  -> [] (* TODO: this case should go away since aliases are handled by the solver structure *)
  | SC_Poly         _  -> []
  | SC_Access_label _  -> []
  | SC_Typeclass   tc -> selector_by_tc repr indexes tc

(* When (αᵢ, …) ∈ { (τ, …) , … } and β = κ(δ …) are in the db,
   aliasing α and β should check if they are non-empty, and in that
   case produce a selector_output for all pairs. This will involve a
   lookup to see if α is constrained by a typeclass
   (typeclasses_constraining indexer). Add to this the logic for
   refined_typeclass vs. typeclass. *)

   (*
    1 , 2 , 3 , 4
    -> (1,3) , (1,4)
  
    +5 -> 1 , 2 , 3 , 4 , 5

   *)

let alias_selector : type_variable -> type_variable -> flds -> selector_output list =
  fun a b (module Indexes) ->
  let a_tcs = (Typeclasses_constraining.get_typeclasses_constraining_list a Indexes.typeclasses_constraining) in
  let b_tcs = (Typeclasses_constraining.get_typeclasses_constraining_list b Indexes.typeclasses_constraining) in
  let a_lhs_constructors = Grouped_by_variable.get_constructors_by_lhs a Indexes.grouped_by_variable in
  let b_lhs_constructors = Grouped_by_variable.get_constructors_by_lhs b Indexes.grouped_by_variable in
  let a_lhs_rows = Grouped_by_variable.get_rows_by_lhs a Indexes.grouped_by_variable in
  let b_lhs_rows = Grouped_by_variable.get_rows_by_lhs b Indexes.grouped_by_variable in
  let a_ctors = MultiSet.map_elements (fun a -> `Constructor a) a_lhs_constructors in
  let a_rows  = MultiSet.map_elements (fun a -> `Row a        ) a_lhs_rows         in
  let b_ctors = MultiSet.map_elements (fun a -> `Constructor a) b_lhs_constructors in
  let b_rows  = MultiSet.map_elements (fun a -> `Row a        ) b_lhs_rows         in
  List.flatten @@
  List.map
    (fun tc ->
       List.map
         (fun c ->
            { tc ; c })
         (a_ctors @ b_ctors @ a_rows @ b_rows ))
    (a_tcs @ b_tcs)

let get_referenced_constraints ({ tc; c } : selector_output) : type_constraint_simpl list =
  [
    SC_Typeclass tc;
    (match c with `Constructor c -> SC_Constructor c | `Row r -> SC_Row r);
  ]

(* ***********************************************************************
 * Propagator
 * *********************************************************************** *)

let restrict_one (cr : constructor_or_row) (allowed : type_value) =
  match cr, allowed.wrap_content with
  | `Constructor { reason_constr_simpl=_; tv=_; c_tag; tv_list }, P_constant { p_ctor_tag; p_ctor_args } ->
    if Compare.constant_tag c_tag p_ctor_tag = 0
    then if List.compare_lengths tv_list p_ctor_args = 0
      then Some p_ctor_args
      else None (* case removed because type constructors are different *)
    else None   (* case removed because argument lists are of different lengths *)
  | `Row _, P_row _ -> failwith "TODO: support P_row similarly to P_constant"
  | _, (P_forall _ | P_variable _ | P_apply _ | P_row _ | P_constant _) -> None (* TODO: does this mean that we can't satisfy these constraints? *)

(* Restricts a typeclass to the possible cases given v = k(a, …) in c *)
let restrict repr (constructor_or_row : constructor_or_row) (tcs : c_typeclass_simpl) =
  let (tv_list, tv) = match constructor_or_row with
    | `Row r -> List.map (fun {associated_variable} -> associated_variable) @@ LMap.to_list r.tv_map , (repr r.tv)
    | `Constructor c -> c.tv_list , (repr c.tv)
  in
  (* TODO: this is bogus if there is shadowing *)
  let index =
    let repr_tv = (repr tv) in
    try List.find_index (fun x -> Compare.type_variable repr_tv (repr x) = 0) tcs.args
    with Failure _ ->
      failwith (Format.asprintf "problem: couldn't find tv = %a in tcs.args = %a"
                  PP.type_variable repr_tv (PP_helpers.list_sep_d PP.type_variable) tcs.args);
  in
  (* Eliminate the impossible cases and splice in the type arguments
     for the possible cases: *)
  let aux allowed_tuple =
    splice_or_none (fun allowed -> restrict_one constructor_or_row allowed) index allowed_tuple in
  let tc = List.filter_map aux tcs.tc in
  (* Replace the corresponding typeclass argument with the type
     variables passed to the type constructor *)
  let args = splice (fun _arg -> tv_list) index tcs.args in
  let id_typeclass_simpl = tcs.id_typeclass_simpl in
  { reason_typeclass_simpl = tcs.reason_typeclass_simpl; original_id = tcs.original_id; id_typeclass_simpl ; tc ; args }

(* input:
     x ? [ map3( nat , unit , float ) ; map3( bytes , mutez , float ) ]
   output:
     true,
     [ x = map( m , n , o ) ; o = float ( ) ],
     [ m ? [ nat  ; bytes ]
       n ? [ unit ; mutez ] ] *)
let replace_var_and_possibilities_1 (repr:type_variable -> type_variable) ((x : type_variable) , (possibilities_for_x : type_value list)) =
  let%bind tags_and_args = bind_map_list get_tag_and_args_of_constant possibilities_for_x in
  let tags_of_constructors, arguments_of_constructors = List.split @@ tags_and_args in
  match all_equal Compare.constant_tag tags_of_constructors with
  | Different ->
    (* The "changed" boolean return indicates whether any update was done.
       It is used to detect when the variable doesn't need any further cleanup. *)
    ok ( false, [ (x, possibilities_for_x) ], [] )            (* Leave as-is, don't deduce anything *)
  | Empty ->
    (* TODO: keep track of the constraints used to refine the
       typeclass so far. *)
    (* fail @@ typeclass_error
     *   "original expected by typeclass"
     *   "actual partially guessed so far (needs a recursive substitution)" *)
    failwith "type error: the typeclass does not allow any type for \
              the variable %a:PP_variable:x at this point"
  | All_equal_to c_tag ->
    match arguments_of_constructors with
    | [] -> failwith "the typeclass does not allow any possibilities \
                      for the variable %a:PP_variable:x at this point"
    | (arguments_of_first_constructor :: _) as arguments_of_constructors ->
      let fresh_vars = List.map (fun _arg -> Core.fresh_type_variable ()) arguments_of_first_constructor in
      let deduced : c_constructor_simpl = {
        id_constructor_simpl = ConstraintIdentifier 0L;
        original_id = None;
        reason_constr_simpl = "inferred because it is the only remaining possibility at this point according to the typeclass [TODO:link to the typeclass here]" ;
        tv = (repr x);
        c_tag ;
        tv_list = fresh_vars
      } in
      (* discard the identical tags, splice their arguments instead, and deduce the x = tag(…) constraint *)
      let sub_part_of_typeclass = {
        reason_typeclass_simpl = Format.asprintf
            "sub-part of a typeclass: expansion of the possible \
             arguments for the constructor associated with %a"
            PP.type_variable (repr x);
        original_id = None;     (* TODO this and the is_mandatory_constraint are not actually used, should use a different type without these fields. *)
        id_typeclass_simpl = ConstraintIdentifier (-1L) ; (* TODO: this and the reason_typeclass_simpl should simply not be used here *)
        args = fresh_vars ;
        tc = arguments_of_constructors ;
      } in
      let%bind possibilities_alist = transpose sub_part_of_typeclass in
      (* The "changed" boolean return indicates whether any update was done.
         It is used to detect when the variable doesn't need any further cleanup. *)
      ok (true, possibilities_alist, [deduced])

let rec replace_var_and_possibilities_rec repr ((x : type_variable) , (possibilities_for_x : type_value list)) =
  let open Rope.SimpleRope in
  let%bind (changed1, possibilities_alist, deduced) = replace_var_and_possibilities_1 repr (x, possibilities_for_x) in
  if changed1 then
    (* the initial var_and_possibilities has been changed, recursively
       replace in the resulting vars and their possibilities, and
       aggregate the deduced constraints. *)
    let%bind (_changed, vp, more_deduced) = replace_vars_and_possibilities_list repr possibilities_alist in
    ok (true, vp, pair (rope_of_list deduced) more_deduced)
  else
    ok (changed1, rope_of_list possibilities_alist, rope_of_list deduced)

and replace_vars_and_possibilities_list repr possibilities_alist =
  let open Rope.SimpleRope in
  bind_fold_list
    (fun (changed_so_far, vps, ds) x ->
       let%bind (changed, vp, d) = replace_var_and_possibilities_rec repr x in
       ok (changed_so_far || changed, pair vps vp, pair ds d))
    (false, empty, empty)
    possibilities_alist

let replace_vars_and_possibilities repr possibilities_alist =
  let open Rope.SimpleRope in
  let%bind (_changed, possibilities_alist, deduced) = replace_vars_and_possibilities_list repr possibilities_alist in
  ok (list_of_rope possibilities_alist, list_of_rope deduced)


type deduce_and_clean_result = {
  deduced : c_constructor_simpl list ;
  cleaned : c_typeclass_simpl ;
}
let deduce_and_clean : (_ -> _) -> c_typeclass_simpl -> (deduce_and_clean_result, _) result = fun repr tcs ->
  Format.printf "In deduce_and_clean for : %a\n%!" PP.c_typeclass_simpl_short tcs;
  (* ex.   [ x                             ; z      ]
       ∈ [ [ map3( nat   , unit  , float ) ; int    ] ;
           [ map3( bytes , mutez , float ) ; string ] ] *)
  let%bind possibilities_alist = transpose tcs in
  (* ex. [ x ? [ map3( nat , unit , float ) ; map3( bytes , mutez , float ) ; ] ;
           z ? [ int                        ; string                        ; ] ; ] *)
  let%bind (vars_and_possibilities, deduced) = replace_vars_and_possibilities repr possibilities_alist in
  (* ex. possibilities_alist:
         [   fresh_x_1 ? [ nat   ; bytes  ] ;
             fresh_x_2 ? [ unit  ; mutez  ] ;
             y         ? [ int   ; string ]     ]
         deduced:
         [ x         = map3  ( fresh_x_1 , fresh_x_2 , fresh_x_3 ) ;
           fresh_x_3 = float (                                   ) ; ] *)
  let%bind cleaned = transpose_back (tcs.reason_typeclass_simpl, tcs.original_id) tcs.id_typeclass_simpl vars_and_possibilities in
  ok { deduced ; cleaned }

let propagator : (selector_output, typer_error) Type_variable_abstraction.Solver_types.propagator =
  fun selected repr ->
  (* The selector is expected to provide constraints with the shape (α
     = κ(β, …)) and to update the private storage to keep track of the
     refined typeclass *)
  let () = Format.printf "and tv: %a and repr tv :%a \n%!" (PP_helpers.list_sep_d PP.type_variable) selected.tc.args (PP_helpers.list_sep_d PP.type_variable) @@ List.map repr selected.tc.args in
  let restricted = restrict repr selected.c selected.tc in
  let () = Format.printf "restricted: %a\n!" PP.c_typeclass_simpl_short restricted in
  let%bind {deduced ; cleaned} = deduce_and_clean repr restricted in
  (* TODO: this is because we cannot return a simplified constraint,
     and instead need to retun a constraint as it would appear if it
     came from the module (generated by the ill-named module
     "Wrap"). type_constraint_simpl is more or less a subset of
     type_constraint, but some parts have been shuffled
     around. Hopefully this can be sorted out so that we don't need a
     dummy value for the srcloc and maybe even so that we don't need a
     conversion (one may dream). *)
  let tc_args = List.map (fun x -> wrap (Todo "no idea") @@ P_variable (repr x)) cleaned.args in
  let cleaned : type_constraint = {
      reason = cleaned.reason_typeclass_simpl;
      c = C_typeclass {
        tc_args ;
        typeclass = cleaned.tc;
        original_id = selected.tc.original_id;
      }
    }
  in
  let aux (x : c_constructor_simpl) : type_constraint = {
    reason = "inferred: only possible type for that variable in the typeclass";
    c = C_equation {
      aval = wrap (Todo "?") @@ P_variable (repr x.tv) ;
      bval = wrap (Todo "? generated") @@
              P_constant {
                p_ctor_tag  = x.c_tag ;
                p_ctor_args = List.map
                  (fun v -> wrap (Todo "? probably generated") @@ P_variable (repr v))
                  x.tv_list ;
              }
      }
    }
  in
  let deduced : type_constraint list = List.map aux deduced in
  let ret = [
      {
        remove_constraints = [SC_Typeclass selected.tc];
        add_constraints = cleaned :: deduced;
        proof_trace = Axiom (HandWaved "cut with the following (cleaned => removed_typeclass) to show that the removal does not lose info, (removed_typeclass => selected.c => cleaned) to show that the cleaned vesion does not introduce unwanted constraints.")
      }
    ] in
  ok ret

(* ***********************************************************************
 * Heuristic
 * *********************************************************************** *)

let printer ppd (t : selector_output) =
  let open Format in
  let open Type_variable_abstraction.PP in
  let lst = t.tc in
  let a = t.c in fprintf ppd "%a and %a" c_typeclass_simpl_short lst constructor_or_row_short a

let pp_deduce_and_clean_result ppf {deduced;cleaned} =
  let open Format in
  let open Type_variable_abstraction.PP in
  fprintf ppf "{@[<hv 2>@
              deduced : %a;@
              cleaned : %a;@
              @]}"
    (PP_helpers.list_sep_d c_constructor_simpl) deduced
    c_typeclass_simpl cleaned

let printer_json (t : selector_output) =
  let open Type_variable_abstraction.Yojson in
  let lst = t.tc in
  let a = t.c in 
  `Assoc [
    ("tc",c_typeclass_simpl lst)
    ;("a",constructor_or_row a)]
let comparator { tc=a1; c=a2 } { tc=b1; c=b2 } =
  let open Type_variable_abstraction.Compare in
  c_typeclass_simpl a1 b1 <? fun () -> constructor_or_row a2 b2
end

module MM = M(Solver_types.Type_variable)(Solver_types.Opaque_type_variable)



open Ast_typed.Types
open Solver_types

module Compat = struct
  module All_plugins = Database_plugins.All_plugins.M(Solver_types.Type_variable)(Solver_types.Opaque_type_variable)
  open All_plugins
  let heuristic_name = MM.heuristic_name
  let selector repr c flds =
    let module Flds = struct
      let grouped_by_variable : type_variable Grouped_by_variable.t = flds#grouped_by_variable
      let assignments : type_variable Assignments.t = flds#assignments
      let typeclasses_constraining : type_variable Typeclasses_constraining.t = flds#typeclasses_constraining
      let by_constraint_identifier : type_variable By_constraint_identifier.t = flds#by_constraint_identifier
    end
    in
    MM.selector repr c (module Flds)
  let alias_selector a b flds =
    let module Flds = struct
      let grouped_by_variable : type_variable Grouped_by_variable.t = flds#grouped_by_variable
      let assignments : type_variable Assignments.t = flds#assignments
      let typeclasses_constraining : type_variable Typeclasses_constraining.t = flds#typeclasses_constraining
      let by_constraint_identifier : type_variable By_constraint_identifier.t = flds#by_constraint_identifier
    end
    in
    MM.alias_selector a b (module Flds)
  let get_referenced_constraints = MM.get_referenced_constraints
  let propagator = MM.propagator
  let printer = MM.printer
  let printer_json = MM.printer_json
  let comparator = MM.comparator
end
let heuristic = Heuristic_plugin Compat.{ heuristic_name; selector; alias_selector; get_referenced_constraints; propagator; printer; printer_json; comparator }

type nonrec deduce_and_clean_result = MM.deduce_and_clean_result = {
    deduced : c_constructor_simpl list ;
    cleaned : c_typeclass_simpl ;
  }
let restrict = MM.restrict
let deduce_and_clean = MM.deduce_and_clean
let pp_deduce_and_clean_result = MM.pp_deduce_and_clean_result
