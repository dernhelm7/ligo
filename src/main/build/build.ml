
open Trace
open Main_errors

module type Toto = sig
  val raise : all raise
  val add_warning : Main_warnings.all -> unit
  val options : Compiler_options.t
end

module M (Toto : Toto) =
  struct
    let raise = Toto.raise
    let add_warning = Toto.add_warning
    let options = Toto.options
    type file_name = string
    type module_name = string
    type compilation_unit = Buffer.t
    type meta_data = Ligo_compile.Helpers.meta
    let preprocess : file_name -> compilation_unit * meta_data * (file_name * module_name) list =
      fun file_name ->
      let meta = trace ~raise build_error_tracer @@ Ligo_compile.Of_source.extract_meta "auto" file_name in
      let c_unit, deps = trace ~raise build_error_tracer @@ Ligo_compile.Helpers.preprocess_file ~meta ~options file_name in
      c_unit,meta,deps
    module AST = struct
      type declaration = Ast_typed.declaration_loc
      type t = declaration list
      type environment = Ast_typed.environment
      let add_to_env : environment -> module_name -> environment -> environment =
        fun env module_name ast_typed_env ->
        Ast_typed.Environment.add_module module_name ast_typed_env env
      let get_env : unit -> environment =
        fun () -> options.init_env
      let make_module_declaration : module_name -> t -> declaration =
        fun module_binder ast_typed ->
        (Location.wrap @@ (Ast_typed.Declaration_module {module_binder;module_=Ast_typed.Module_Fully_Typed ast_typed}: Ast_typed.declaration))
      let make_module_alias : module_name -> file_name -> declaration =
        fun module_name file_name ->
        Location.wrap @@ (Ast_typed.Module_alias {alias=module_name;binders=file_name,[]}: Ast_typed.declaration)
    end
    let compile : AST.environment -> file_name -> meta_data -> compilation_unit -> (AST.t * AST.environment) =
      fun env file_name meta c_unit ->
      let options = {options with init_env = env } in
      let ast_core = Ligo_compile.Utils.to_core ~raise ~add_warning ~options ~meta c_unit file_name in
      let inferred = Ligo_compile.Of_core.infer ~raise ~options ast_core in
      let Ast_typed.Module_Fully_Typed ast_typed,ast_typed_env = Ligo_compile.Of_core.typecheck ~raise ~add_warning ~options Ligo_compile.Of_core.Env inferred in
      (ast_typed,ast_typed_env)

  end

module Infer (Toto : Toto) = struct
  include M(Toto)
  module AST = struct
    include AST
    type declaration = Ast_core.declaration Location.wrap
    type t = declaration list
      type environment = Ast_typed.environment
      let add_to_env : environment -> module_name -> environment -> environment =
        fun env module_name ast_typed_env ->
        Ast_typed.Environment.add_module module_name ast_typed_env env
      let get_env : unit -> environment =
        fun () -> options.init_env
      let make_module_declaration : module_name -> t -> declaration =
        fun module_binder ast_typed ->
        (Location.wrap @@ (Ast_core.Declaration_module {module_binder;module_=ast_typed}: Ast_core.declaration))
      let make_module_alias : module_name -> file_name -> declaration =
        fun module_name file_name ->
        Location.wrap @@ (Ast_core.Module_alias {alias=module_name;binders=file_name,[]}: Ast_core.declaration)
  end

  let compile : AST.environment -> file_name -> meta_data -> compilation_unit -> (AST.t * AST.environment) =
    fun env file_name meta c_unit ->
    let options = {options with init_env = env } in
    let ast_core = Ligo_compile.Utils.to_core ~raise ~add_warning ~options ~meta c_unit file_name in
    let inferred = Ligo_compile.Of_core.infer ~raise ~options ast_core in
    (inferred,env)

end

module Build(Toto : Toto) = BuildSystem.Make(M(Toto))

type file_name = string

let dependency_graph ~raise ~add_warning : options:Compiler_options.t -> string -> Ligo_compile.Of_core.form -> file_name -> _ =
  fun ~options _syntax _form file_name ->
    let open Build(struct
      let raise = raise
      let add_warning = add_warning
      let options = options
    end) in
    dependency_graph file_name

let infer_contract ~raise ~add_warning : options:Compiler_options.t -> string -> Ligo_compile.Of_core.form -> file_name -> _ =
  fun ~options _syntax _entry_point main_file_name ->
    let open BuildSystem.Make(Infer(struct
      let raise = raise
      let add_warning = add_warning
      let options = options
    end)) in
    let infered,_ = trace ~raise (build_corner_case __LOC__) @@ from_result (compile_separate main_file_name) in
    infered

let type_contract ~raise ~add_warning : options:Compiler_options.t -> string -> Ligo_compile.Of_core.form -> file_name -> _ =
  fun ~options _syntax _entry_point file_name ->
    let open Build(struct
      let raise = raise
      let add_warning = add_warning
      let options = options
    end) in
    let contract,env = trace ~raise (build_corner_case __LOC__) @@ from_result (compile_separate file_name) in
    Ast_typed.Module_Fully_Typed contract, env

let combined_contract ~raise ~add_warning : options:Compiler_options.t -> _ -> _ -> file_name -> _ =
  fun ~options _syntax _entry_point file_name ->
    let open Build(struct
      let raise = raise
      let add_warning = add_warning
      let options = options
    end) in
    let contract,env = trace ~raise (build_corner_case __LOC__) @@ from_result (compile_combined file_name) in
    Ast_typed.Module_Fully_Typed contract, env

let build_mini_c ~raise ~add_warning : options:Compiler_options.t -> _ -> _ -> file_name -> _ =
  fun ~options _syntax _entry_point file_name ->
    let open Build(struct
      let raise = raise
      let add_warning = add_warning
      let options = options
    end) in
    let contract,env = trace ~raise (build_corner_case __LOC__) @@ Trace.from_result (compile_combined file_name) in
    let mini_c       = trace ~raise build_error_tracer @@ Ligo_compile.Of_typed.compile (Ast_typed.Module_Fully_Typed contract) in
    (mini_c,env)

let build_expression ~raise ~add_warning : options:Compiler_options.t -> string -> _ -> file_name option -> _ =
  fun ~options syntax expression file_name ->
    let (module_,env) = match file_name with
      | Some init_file ->
         let contract, env = combined_contract ~raise ~add_warning ~options syntax Ligo_compile.Of_core.Env init_file in
         (contract, env)
      | None -> (Module_Fully_Typed [],options.init_env) in
    let typed_exp,_     = Ligo_compile.Utils.type_expression ~raise ~options file_name syntax expression env in
    let data, typed_exp = Self_ast_typed.monomorphise_expression typed_exp in
    let _, module_      = Self_ast_typed.monomorphise_module_data data module_ in
    let decl_list       = trace ~raise build_error_tracer @@ Ligo_compile.Of_typed.compile module_ in
    let mini_c_exp      = Ligo_compile.Of_typed.compile_expression ~raise typed_exp in
    mini_c_exp, decl_list
let build_contract ~raise ~add_warning : options:Compiler_options.t -> string -> _ -> file_name -> _ =
  fun ~options syntax entry_point file_name ->
    let mini_c,_   = build_mini_c ~raise ~add_warning ~options syntax (Ligo_compile.Of_core.Contract entry_point) file_name in
    let michelson  = trace ~raise build_error_tracer @@ Ligo_compile.Of_mini_c.aggregate_and_compile_contract ~options mini_c entry_point in
    michelson

let build_contract_use ~raise ~add_warning : options:Compiler_options.t -> string -> file_name -> _ =
  fun ~options _syntax file_name ->
    let open Build(struct
      let raise = raise
      let add_warning = add_warning
      let options = options
    end) in
    let contract,env = trace ~raise (build_corner_case __LOC__) @@ Trace.from_result (compile_combined file_name) in
    let mini_c,map   = trace ~raise build_error_tracer @@ Ligo_compile.Of_typed.compile_with_modules (Ast_typed.Module_Fully_Typed contract) in
    (mini_c, map, Ast_typed.Module_Fully_Typed contract, env)

let build_contract_module ~raise ~add_warning : options:Compiler_options.t -> string -> _ -> file_name -> file_name -> _ =
  fun ~options _syntax _entry_point file_name module_name ->
    let open Build(struct
      let raise = raise
      let add_warning = add_warning
      let options = options
    end) in
  let contract,env = trace ~raise (build_corner_case __LOC__) @@ Trace.from_result (compile_combined file_name) in
  let module_contract = Ast_typed.Declaration_module { module_binder = module_name;
                                                      module_ = Ast_typed.Module_Fully_Typed contract } in
  let contract = Ast_typed.Module_Fully_Typed [Location.wrap module_contract] in
  let mini_c,map = trace ~raise build_error_tracer @@ Ligo_compile.Of_typed.compile_with_modules contract in
  (mini_c, map, contract, env)
