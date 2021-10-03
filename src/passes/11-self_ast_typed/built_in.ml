(* This module identifies the usage of built-in modules like String.length, Crypto.sha256 etc.
 * and patches in the missing declarations, by doing so the rest of the pipeline will work as is
 * The next step step in the pipeline 12-spilling will convert the module declarations to records
 * and add it to the mini-c environment *)
 
module DeclarationMap = Map.Make(String)

module AccessSet = Set.Make(struct 
  type t = string * Ast_typed.expression_variable
  let compare = fun (m1, b1) (m2, b2) -> 
    let s = String.compare m1 m2 in
    if s = 0 then Var.compare (Location.unwrap b1) (Location.unwrap b2)
    else s
end)

type decl_kind = 
  | Declaration
  | ModuleRef of string

(* We go travers the typed AST and look at all declarations
 * and maintain a map (key: string ; value: decl_kind)
 * here the string represents the module declaration path 
 * e.g. 
  module A = struct
    module B = struct
      module C = struct
        (* declationAccessSet... *)
      end
    end
  end
  module X = A.B

 * will have an map like 
 * { "A": Declaration, "A.B": Delclaration, "A.B.C": Declaration, "X": ModuleRef "A.B" } *)

(* Then we traverse typed AST again to look for module accesses
 * and maintain a set (elt: (string * expression_variable))
 * the string in the tuple represents the module access path
 * e.g. let _ = A.B.C.x
 * the above expression will have an entry in the set like ("A.B.C", expression_variable(x)) *)

 (* After constructing the declarations map & module accesses set
  * We fold over the set and look for entries in the map
  * basically check that each module access has a corresponding declaration
  * Note. for the case of module alias (ModuleRef) we try to resolve to a declaration by recursively looking in the map
  * All module accesses for which we cannot find entries in the declaration map are Built-in modules *)

(* For the Built-in modules we add only the declarations that used
 * e.g. if only String.length is used, then we add a module declation for String
 * and only declaration for length *)

(* 
  Take into account these cases: 

  module X = struct
    module Y = struct
      module Z = List
    end
  end

  module A = X.Y.Z (* This points to built-in List *)

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

let rec get_all_module_accesses_expr (module_name : string) (expr : Ast_typed.expression) = 
  match expr.expression_content with
  | E_literal _  -> AccessSet.empty
  | E_constant { arguments } ->
    List.fold_left arguments ~init:AccessSet.empty ~f:(fun s arg ->
      let arg = get_all_module_accesses_expr module_name arg in
      AccessSet.union s arg)
  | E_variable v -> AccessSet.singleton (module_name, v)
  | E_application { lamb ; args } -> 
    let lamb = get_all_module_accesses_expr module_name lamb in
    let args = get_all_module_accesses_expr module_name args in
    AccessSet.union lamb args
  | E_recursive { lambda = { result } }
  | E_lambda { result } ->
    let result = get_all_module_accesses_expr module_name result in
    result
  | E_let_in { rhs ; let_result } ->
    let rhs        = get_all_module_accesses_expr module_name rhs in
    let let_result = get_all_module_accesses_expr module_name let_result in
    AccessSet.union rhs let_result
  | E_type_in { let_result } ->
    let let_result = get_all_module_accesses_expr module_name let_result in
    let_result
  | E_mod_in { rhs ; let_result } -> 
    let rhs        = get_all_module_accesses module_name rhs in
    let let_result = get_all_module_accesses_expr module_name let_result in
    AccessSet.union rhs let_result
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
          List.fold_left cases ~init:AccessSet.empty ~f:(fun s { body } ->
            let body = get_all_module_accesses_expr module_name body in
            AccessSet.union s body)
      | Match_record { body } -> 
        let body = get_all_module_accesses_expr module_name body in
        body in
    AccessSet.union matchee cases
  | E_record fields ->
    Ast_typed.LMap.fold (fun _ e s ->
      let e = get_all_module_accesses_expr module_name e in
      AccessSet.union s e
    ) fields AccessSet.empty 
  | E_record_accessor { record } ->
    let record = get_all_module_accesses_expr module_name record in
    record
  | E_record_update { record ; update } ->
    let record = get_all_module_accesses_expr module_name record in
    let update = get_all_module_accesses_expr module_name update in
    AccessSet.union record update
  | E_module_accessor { module_name = module_name' ; element } ->
    let module_name = module_name ^ "." ^ module_name' in
    let element     = get_all_module_accesses_expr module_name element in
    element
and get_all_module_accesses (module_name : string) (Module_Fully_Typed decls : Ast_typed.module_fully_typed) =
  List.fold_left decls ~init:AccessSet.empty ~f:(fun s decl ->
    let decl = Location.unwrap decl in
    match decl with
    | Declaration_constant { expr } ->
      AccessSet.union s (get_all_module_accesses_expr module_name expr)
    | Declaration_module { module_ } ->
      AccessSet.union s (get_all_module_accesses module_name module_)
    | Declaration_type _ -> s
    | Module_alias _     -> s
    )

open Ast_typed
let trace_option = Simple_utils.Trace.trace_option
let built_in_modules = ["String";"Crypto"]

let rec get_all_module_declarations (Module_Fully_Typed decls : module_fully_typed) =
  let m : decl_kind DeclarationMap.t = DeclarationMap.empty in
  List.fold_left decls ~init:m ~f:(fun m decl ->
    let decl = Location.unwrap decl in
    match decl with
    | Declaration_constant _ -> m
    | Declaration_type _     -> m
    | Declaration_module { module_binder ; module_ } ->
      let m = DeclarationMap.add module_binder Declaration m in
      let n = get_all_module_declarations module_ in
      DeclarationMap.fold (fun k v acc ->
        DeclarationMap.add (module_binder ^ "." ^ k) v acc
      ) m n
    | Module_alias { alias ; binders } -> 
      let binders = String.concat "." @@ List.Ne.to_list binders in
      DeclarationMap.add alias (ModuleRef binders) m
    )  

let convert_env_module_to_declations ~raise (used_vars : expression_variable list) module_binder env =
  let expressions = Environment.get_expr_environment env in
  let module_ = Module_Fully_Typed (List.map used_vars ~f:(fun used_var ->
    let used_var = Location.unwrap used_var in
    let environment_binding = 
      trace_option ~raise (Errors.corner_case 
        (Format.asprintf "No Binding found %s.%a in the environment" module_binder Var.pp used_var))
      @@ List.find expressions ~f:(fun { expr_var } -> Var.equal used_var (Location.unwrap expr_var)) in
    let { expr_var ; env_elt } = environment_binding in
      (match env_elt.definition with 
      | ED_declaration {expression} -> 
        let attr : attribute = { inline = false ; no_mutation = false } in
        Location.wrap @@ Declaration_constant { binder = expr_var; expr = expression ; attr ; name = None }
      | ED_binder -> 
        raise.raise @@ Errors.corner_case ""))) in
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
  let module_declarations = get_all_module_declarations (Module_Fully_Typed lst) in

  let built_in_modules_map = AccessSet.fold (fun (m, e) xs ->
    let m = trim_leading_dot m in
    match DeclarationMap.find_opt m module_declarations with
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
      (match SMap.find_opt m xs with
      | Some vars -> 
        SMap.add m (e :: vars) xs
      | None -> SMap.add m [e] xs)
    ) module_accesses SMap.empty in
  let module_decls = List.fold_left built_in_modules ~init:[] ~f:(fun xs m ->
    match SMap.find_opt m built_in_modules_map with
    | Some used_vars ->
      let module_env = trace_option 
        ~raise (Errors.corner_case "Built-in module not present in environment") 
        @@ Environment.get_module_opt m env in
  
      let decls = convert_env_module_to_declations ~raise used_vars m module_env in
      decls @ xs
    | None -> xs (* impossible since we reached so far in the pipeline*)
    ) in
  (* TODO: handle top-level constants like amount, blake2b, etc. *)
  let lst = module_decls @ lst in
  Module_Fully_Typed lst
