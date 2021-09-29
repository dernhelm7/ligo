
module M = Map.Make(String)

(* We go through the Typed AST and look for module accesses
   and maintain a map of module name again list of variables (accesses)
*)

let get_module_accesses module_ =
  Helpers.fold_module_decl
    (fun acc e ->
      let open Ast_typed in
      match e.expression_content with
      | E_module_accessor {module_name; element={expression_content=E_variable v}} ->
        if M.mem module_name acc
        then
          let vars = M.find module_name acc in
          M.add module_name (v::vars) acc
        else
          M.add module_name [v] acc
      | _ -> acc)
    (fun acc _ -> acc)
    M.empty
    module_

open Ast_typed
let built_in_modules = ["String"]

let convert_env_module_to_declations (used_vars : expression_variable list) module_binder env =
  let expressions = Environment.get_expr_environment env in
  let module_ = Module_Fully_Typed (List.filter_map ~f:(fun {expr_var;env_elt} -> 
    let is_decl_used = Option.is_some @@ List.find used_vars ~f:(fun v ->
      Var.equal v.wrap_content expr_var.wrap_content) in
    if is_decl_used then
    match env_elt.definition with 
    | ED_declaration {expression} -> 
      let attr : attribute = { inline = false ; no_mutation = false } in
      Some (Location.wrap @@ Declaration_constant { binder = expr_var; expr = expression ; attr ; name = None })
    | ED_binder -> None
    else None
  ) expressions)
  in
  [Location.wrap @@ Declaration_module {module_binder;module_}]

let add_built_in_modules ~raise ((Module_Fully_Typed lst) : module_fully_typed) env = 
  let module_accesses = get_module_accesses (Module_Fully_Typed lst) in
  let module_decls = built_in_modules 
    |> List.map ~f:(fun module_name ->
      let is_used = M.mem module_name module_accesses in
      let module_env,built_in = Simple_utils.Trace.trace_option 
        ~raise (Errors.corner_case "Built-in module not present in environment") 
        @@ Environment.get_module_and_built_in_flag_opt module_name env in
      if built_in && is_used
      then
        let used_vars = M.find module_name module_accesses in
        let module_decl = convert_env_module_to_declations used_vars module_name module_env in
        module_decl
      else []) 
    |> List.concat
  in
  let lst = module_decls @ lst in
  Module_Fully_Typed lst
