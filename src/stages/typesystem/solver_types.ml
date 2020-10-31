open Trace
open Ast_typed.Types
module Set = RedBlackTrees.PolySet

type 'old_constraint_type selector_input = 'old_constraint_type (* some info about the constraint just added, so that we know what to look for *)
type 'selector_output selector_outputs = 'selector_output list
(* type ('old_contraint_type, 'selector_output) selector = 'old_constraint_type selector_input -> structured_dbs -> 'selector_output selector_outputs *)
type ('selector_output , 'errors) propagator = 'selector_output -> (updates, 'errors) result

type ('selector_output, -'flds) selector = type_constraint_simpl -> 'flds -> 'selector_output list

type ('selector_output, -'flds) heuristic_plugin = {
  selector     : ('selector_output, 'flds) selector ;
  propagator   : ('selector_output, Ast_typed.Typer_errors.typer_error) propagator ;
  printer      : Format.formatter -> 'selector_output -> unit ;
  printer_json : 'selector_output -> Yojson.Safe.t ;
  comparator   : 'selector_output -> 'selector_output -> int ;
}

type ('selector_output, -'flds) heuristic_state = {
  plugin : ('selector_output, 'flds) heuristic_plugin ;
  already_selected : 'selector_output Set.t ;
}

type -'flds ex_heuristic_plugin =
    Heuristic_plugin : ('selector_output, 'flds) heuristic_plugin -> 'flds ex_heuristic_plugin

type -'flds ex_heuristic_state =
    Heuristic_state : ('selector_output, 'flds) heuristic_state -> 'flds ex_heuristic_state

type 'flds heuristic_plugins = 'flds ex_heuristic_plugin list

type 'flds heuristic_states = 'flds ex_heuristic_state list

module type Plugins = sig
  module Indexers : IndexerPlugins
  val heuristics : Indexers.PluginFields(PerPluginState).flds heuristic_plugins
end


(* type ('old_constraint_type , 'selector_output , 'errors) propagator_heuristic = {
 *   (\* sub-sub component: lazy selector (don't re-try all selectors every time)
 *    * For now: just re-try everytime *\)
 *   selector          : ('old_constraint_type , 'selector_output) selector ;
 *   (\* constraint propagation: (buch of constraints) → (new constraints * assignments) *\)
 *   propagator        : ('selector_output , 'errors) propagator ;
 *   printer           : Format.formatter -> 'selector_output -> unit ;
 *   printer_json      : 'selector_output -> Yojson.Safe.t ;
 *   comparator        : 'selector_output -> 'selector_output -> int ;
 * }
 * 
 * type ('old_constraint_type , 'selector_output, 'errors) propagator_state = {
 *   selector          : ('old_constraint_type , 'selector_output) selector ;
 *   propagator        : ('selector_output , 'errors) propagator ;
 *   printer           : Format.formatter -> 'selector_output -> unit ;
 *   printer_json      : 'selector_output -> Yojson.Safe.t ;
 *   already_selected  : 'selector_output Set.t;
 * } *)

(* type 'errors ex_propagator_heuristic =
 *   (\* For now only support a single type of input, make this polymorphic as needed. *\)
 *   | Propagator_heuristic : (type_constraint_simpl , 'selector_output , 'errors) propagator_heuristic -> 'errors ex_propagator_heuristic *)

(* type 'errors ex_propagator_state =
 *   (\* For now only support a single type of input, make this polymorphic as needed. *\)
 *   | Propagator_state : (type_constraint_simpl , 'selector_output , 'errors) propagator_state -> 'errors ex_propagator_state *)

(* type 'errors typer_state = {
 *   structured_dbs                   : structured_dbs   ;
 *   already_selected_and_propagators : 'errors ex_propagator_state list ;
 * } *)

type ('errors, 'plugin_states) __plugins__typer_state = {
  all_constraints_                  : type_constraint_simpl list ;
  aliases_                          : type_variable UnionFind.Poly2.t ;
  plugin_states                    : 'plugin_states ;
  already_selected_and_propagators_ : 'plugin_states ex_heuristic_state list ;
}

open Format
open PP_helpers

let pp_already_selected = fun printer ppf set ->
  let lst = (RedBlackTrees.PolySet.elements set) in
    Format.fprintf ppf "Set [@,@[<hv 2> %a @]@,]" (list_sep printer (fun ppf () -> fprintf ppf " ;@ ")) lst

(* let pp_ex_propagator_state = fun ppf (Propagator_state { selector ; propagator ; printer ; printer_json=_ ; already_selected }) ->
 *   ignore ( selector, propagator );
 *   Format.fprintf ppf "{ selector = (\* OCaml function *\); propagator = (\* OCaml function *\); already_selected = %a }"
 *   (pp_already_selected printer) already_selected *)

(* let pp_typer_state = fun ppf ({ structured_dbs; already_selected_and_propagators } : _ typer_state) ->
 *   Format.fprintf ppf "{ structured_dbs = %a ; already_selected_and_propagators = [ %a ] }"
 *     Ast_typed.PP.structured_dbs structured_dbs
 *     (list_sep pp_ex_propagator_state (fun ppf () -> fprintf ppf " ;@ ")) already_selected_and_propagators *)


let json_already_selected = fun printer_json set : Yojson.Safe.t ->
  let lst = (RedBlackTrees.PolySet.elements set) in
let list f lst = `List (List.map f lst) in
    `List [`String "Set"; (list printer_json lst)]

let json_ex_propagator_state = fun (Heuristic_state { plugin = { selector; propagator; printer=_ ; printer_json } ; already_selected }) : Yojson.Safe.t ->
  ignore (selector,propagator);
  `Assoc[ ("selector", `String "OCaml function"); ("propagator", `String "OCaml function"); ("already_selected" ,          (json_already_selected printer_json) already_selected)]

(* TODO: remove lift_state_list_monad and lift: not needed after moving to plugin system *)
(* state+list monad *)
type ('state, 'elt) state_list_monad = { state: 'state ; list : 'elt list }
let lift_state_list_monad ~state ~list = { state ; list }
let lift f =
  fun { state ; list } ->
    let (new_state , new_lists) = List.fold_map_acc f state list in
    { state = new_state ; list = List.flatten new_lists }
