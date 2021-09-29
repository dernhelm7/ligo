open Ast_typed

let built_in_modules = ["String"]

let convert_env_module_to_declations module_binder env =
  let expressions = Environment.get_expr_environment env in
  let module_ = Module_Fully_Typed (List.map ~f:(fun {expr_var;env_elt} -> 
    match env_elt.definition with 
    | ED_declaration {expression} -> 
      let attr : attribute = { inline = false ; no_mutation = false } in
      Location.wrap @@ Declaration_constant { binder = expr_var; expr = expression ; attr ; name = None }
    | ED_binder -> failwith "not possbile"
  ) expressions)
  in
  [Location.wrap @@ Declaration_module {module_binder;module_}]

let add_built_in_modules ~raise ((Module_Fully_Typed lst) : module_fully_typed) env = 
  let module_decls = built_in_modules 
    |> List.map ~f:(fun module_name ->
      let module_env,built_in = Simple_utils.Trace.trace_option 
        ~raise (Errors.corner_case "Built-in module not present in environment") 
        @@ Environment.get_module_and_built_in_flag_opt module_name env in
      if built_in 
      then
        let module_decl = convert_env_module_to_declations module_name module_env in
        module_decl
      else []) 
    |> List.concat
  in
  let lst = module_decls @ lst in
  Module_Fully_Typed lst
