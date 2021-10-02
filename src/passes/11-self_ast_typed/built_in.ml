(* TODO: write nice description about what this module does *)
module M = Map.Make(String)

module S = Set.Make(struct 
  type t = string * Ast_typed.expression_variable
  let compare = fun (m1, b1) (m2, b2) -> 
    let s = String.compare m1 m2 in
    if s = 0 then Var.compare (Location.unwrap b1) (Location.unwrap b2)
    else s
end)

module S' = S

type decl_kind = 
  | Declaration
  | ModuleRef of string

(* We go through the Typed AST and look for module accesses
   and maintain a map of module name again list of variables (accesses)
*)
(* 
  Take into account these cases: 

  module X = struct
    module Y = struct
      module Z = List
    end
  end

  module A = X.Y.Z (* This points to List *)

  let _ = A.map (fun x -> x) []

  let _ = amount (* This is a predefined constant *)
  (* Handle this via free variable *)

  module A = struct
    module B = struct
      module C = struct
        let x = 1
      end
    end
  end

  module D = A.B

  let _ = D.C.x 

*)

(* TODO: Remove function binders *)
let rec get_all_module_accesses_expr (module_name : string) (expr : Ast_typed.expression) = 
  match expr.expression_content with
  | E_literal _  -> S.empty
  | E_constant { arguments } ->
    List.fold_left arguments ~init:S.empty ~f:(fun s arg ->
      let arg = get_all_module_accesses_expr module_name arg in
      S.union s arg)
  | E_variable v -> S.singleton (module_name, v)
  | E_application { lamb ; args } -> 
    let lamb = get_all_module_accesses_expr module_name lamb in
    let args = get_all_module_accesses_expr module_name args in
    S.union lamb args
  | E_recursive { lambda = { result } }
  | E_lambda { result } ->
    let result = get_all_module_accesses_expr module_name result in
    result
  | E_let_in { rhs ; let_result } ->
    let rhs        = get_all_module_accesses_expr module_name rhs in
    let let_result = get_all_module_accesses_expr module_name let_result in
    S.union rhs let_result
  | E_type_in { let_result } ->
    let let_result = get_all_module_accesses_expr module_name let_result in
    let_result
  | E_mod_in { rhs ; let_result } -> 
    let rhs        = get_all_module_accesses module_name rhs in
    let let_result = get_all_module_accesses_expr module_name let_result in
    S.union rhs let_result
  | E_mod_alias { result } ->
    let result = get_all_module_accesses_expr module_name result in
    result
  | E_raw_code { code } ->
    let code = get_all_module_accesses_expr module_name code in
    code
  | E_constructor { element } ->
    let element = get_all_module_accesses_expr module_name element in
    element
  | E_matching { matchee ; cases } ->
    let matchee = get_all_module_accesses_expr module_name matchee in
    let cases   = 
      match cases with
      | Match_variant { cases } ->
          List.fold_left cases ~init:S.empty ~f:(fun s { body } ->
            let body = get_all_module_accesses_expr module_name body in
            S.union s body)
      | Match_record { body } -> 
        let body = get_all_module_accesses_expr module_name body in
        body in
    S.union matchee cases
  | E_record fields ->
    Ast_typed.LMap.fold (fun _ e s ->
      let e = get_all_module_accesses_expr module_name e in
      S.union s e
    ) fields S.empty 
  | E_record_accessor { record } ->
    let record = get_all_module_accesses_expr module_name record in
    record
  | E_record_update { record ; update } ->
    let record = get_all_module_accesses_expr module_name record in
    let update = get_all_module_accesses_expr module_name update in
    S.union record update
  | E_module_accessor { module_name = module_name' ; element } ->
    let module_name = module_name ^ "." ^ module_name' in
    let element     = get_all_module_accesses_expr module_name element in
    element
and get_all_module_accesses (module_name : string) (Module_Fully_Typed decls : Ast_typed.module_fully_typed) =
  List.fold_left decls ~init:S.empty ~f:(fun s decl ->
    let decl = Location.unwrap decl in
    match decl with
    | Declaration_constant { expr } ->
      (* print_endline @@ Format.asprintf "%a" Ast_typed.PP.expression expr; *)

      (* TODO: Write this nicely *)
      S'.union s (get_all_module_accesses_expr module_name expr)
    | Declaration_module { module_ } ->
      S'.union s (get_all_module_accesses module_name module_)
    | Declaration_type _ -> s
    | Module_alias _     -> s
    )

open Ast_typed
let built_in_modules = ["String";"Crypto"]

let rec get_all_module_declarations (Module_Fully_Typed decls : module_fully_typed) =
  let m : decl_kind M.t = M.empty in
  List.fold_left decls ~init:m ~f:(fun m decl ->
    let decl = Location.unwrap decl in
    match decl with
    | Declaration_constant _ -> m
    | Declaration_type _     -> m
    | Declaration_module { module_binder ; module_ } ->
      let m = M.add module_binder Declaration m in
      let n = get_all_module_declarations module_ in
      M.fold (fun k v acc ->
        M.add (module_binder ^ "." ^ k) v acc
      ) m n
    | Module_alias { alias ; binders } -> 
      let binders = String.concat "." @@ List.Ne.to_list binders in
      M.add alias (ModuleRef binders) m
    )  

(* TODO: Iterate over used_vars instead of expressions *)
let convert_env_module_to_declations (used_vars : expression_variable list) module_binder env =
  let expressions = Environment.get_expr_environment env in
  (* Printf.printf "module_binder = %s expressions.length =  %d\n" module_binder (List.length expressions); *)
  let module_ = Module_Fully_Typed (List.filter_map ~f:(fun {expr_var;env_elt} -> 
    (* Format.printf "expr_var %a\n" Var.pp expr_var.wrap_content; *)
    let is_decl_used = Option.is_some @@ List.find used_vars ~f:(fun v ->
      Var.equal v.wrap_content expr_var.wrap_content) in
    (* Printf.printf "is_decl_used %B\n" is_decl_used; *)
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

let rec resolve_module (module_name : string) (module_declarations : decl_kind SMap.t) =
  match SMap.find_opt module_name module_declarations with
  | Some Declaration -> None
  | Some (ModuleRef m) -> resolve_module m module_declarations
  | None -> Some module_name

let trim_leading_dot s = 
  let len = String.length s in
  if len = 0 then s
  else String.sub s 1 (len - 1)

let add_built_in_modules ~raise ((Module_Fully_Typed lst) : module_fully_typed) env = 
  let module_accesses = get_all_module_accesses "" (Module_Fully_Typed lst) in
  (* let _ = S'.iter (fun (m,v) ->
    print_endline @@ Format.asprintf " '%s' %a" m Var.pp (Location.unwrap v)
    ) module_accesses in *)
  let module_declarations = get_all_module_declarations (Module_Fully_Typed lst) in

  let built_in_modules_map = S'.fold (fun (m, e) xs ->
    let m = trim_leading_dot m in
    match M.find_opt m module_declarations with
    | Some Declaration -> xs
    | Some (ModuleRef m) ->
      (match resolve_module m module_declarations with
      | Some m -> 
        print_endline m;
        (match SMap.find_opt m xs with
        | Some vars -> SMap.add m (e :: vars) xs
        | None -> SMap.add m [e] xs)
      | None -> xs)
    | None ->
      (* print_endline m; *)
      (* print_endline @@ Format.asprintf "e = %a\n" Var.pp e.wrap_content; *)
      (match SMap.find_opt m xs with
      | Some vars -> 
        (* List.iter vars ~f:(fun used_var -> print_endline @@ Format.asprintf "vars = %a" Var.pp (Location.unwrap used_var)); *)
        SMap.add m (e :: vars) xs
      | None -> SMap.add m [e] xs)
    ) module_accesses SMap.empty in
  let module_decls = List.fold_left built_in_modules ~init:[] ~f:(fun xs m ->
    match SMap.find_opt m built_in_modules_map with
    | Some used_vars ->
      let module_env,_ = Simple_utils.Trace.trace_option 
        ~raise (Errors.corner_case "Built-in module not present in environment") 
        @@ Environment.get_module_and_built_in_flag_opt m env in
        
      (* List.iter used_vars ~f:(fun used_var -> print_endline @@ Format.asprintf "%a" Var.pp (Location.unwrap used_var)); *)
      
      let decls = convert_env_module_to_declations used_vars m module_env in
      decls @ xs
    | None -> xs (* impossible since we reached so far in the pipeline*)
    ) in
  let lst = module_decls @ lst in
  Module_Fully_Typed lst
