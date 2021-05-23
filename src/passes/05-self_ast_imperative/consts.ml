open Helpers
open Errors
open Ast_imperative
open Trace

let is_const = fun x -> match x with
                      | { const_or_var = Some `Const } -> true
                      | _ -> false

let add_binder b var vars =
  let vars = remove_from var vars in
  if b then var :: vars else vars

let add_binders binders vars =
  let vars = List.fold_right remove_from binders vars in
  binders @ vars

let rec assign_expression : ?vars:expression_variable list -> expression -> (expression , self_ast_imperative_error) result = fun ?(vars = []) e ->
  let self = assign_expression in
  let* _ = fold_map_expression
                 (fun (vars : expression_variable list) expr ->
                   match expr.expression_content with
                   | E_assign {variable} when in_vars variable vars ->
                      fail @@ const_rebound variable
                   | E_lambda {binder={var;attributes}} ->
                      let vars = add_binder (is_const attributes) var vars in
                      ok (true, vars, expr)
                   | E_let_in {let_binder={var;attributes};rhs;let_result} ->
                      let* _ = self ~vars rhs in
                      let vars = add_binder (is_const attributes) var vars in
                      let* _ = self ~vars let_result in
                      ok (false, vars, expr)
                   | E_matching {matchee;cases} ->
                      let f {pattern;body} =
                        let all_pattern_vars = get_pattern pattern in
                        let vars = List.fold_right remove_from all_pattern_vars vars in
                        let const_pattern_vars = get_pattern ~pred:is_const pattern in
                        let vars =
                          List.fold_right (add_binder true) const_pattern_vars vars in
                        self ~vars body in
                      let* _ = self ~vars matchee in
                      let* _ = bind_map_list f cases in
                      ok (false, vars, expr)
                   | E_recursive {lambda={binder={var;attributes}}} ->
                      let vars = add_binder (is_const attributes) var vars in
                      ok (true, vars, expr)
                   | _  ->
                      ok (true, vars, expr)
                   ) vars e in
  ok e

let assign_expression : expression -> (expression , self_ast_imperative_error) result =
  assign_expression
