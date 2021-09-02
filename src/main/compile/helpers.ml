open Trace
open Main_errors

type s_syntax = Syntax_name of string
type v_syntax = PascaLIGO | CameLIGO | ReasonLIGO | JsLIGO

type meta = {
  syntax : v_syntax;
}

let protocol_to_variant ~raise : string -> Environment.Protocols.t =
  fun s ->
  trace_option ~raise (invalid_protocol_version Environment.Protocols.protocols_str s)
  @@ Environment.Protocols.protocols_to_variant s

let get_initial_env ~raise : ?test_env:bool -> string -> Ast_typed.environment = fun ?(test_env=false) protocol_as_str ->
  let protocol = protocol_to_variant ~raise protocol_as_str in
  (if test_env then Environment.default_with_test else Environment.default) protocol

(*TODO : move this function to src/helpers so that src/build/.. can use it *)
let file_extension_to_variant sf =
  match sf with
  | ".ligo" | ".pligo" -> Some PascaLIGO
  | ".mligo"           -> Some CameLIGO
  | ".religo"          -> Some ReasonLIGO
  | ".jsligo"          -> Some JsLIGO
  | _                  -> None

let syntax_to_variant ~raise (Syntax_name syntax) source =
  match syntax, source with
  | "auto", Some sf ->
    let sf = Filename.extension sf in
    trace_option ~raise (syntax_auto_detection sf) @@
      file_extension_to_variant sf
  | ("pascaligo" | "PascaLIGO"),   _ -> PascaLIGO
  | ("cameligo" | "CameLIGO"),     _ -> CameLIGO
  | ("reasonligo" | "ReasonLIGO"), _ -> ReasonLIGO
  | ("jsligo" | "JsLIGO"),         _ -> JsLIGO
  | _ -> raise.raise (invalid_syntax syntax)

let variant_to_syntax v =
  match v with
  | PascaLIGO -> "pascaligo"
  | CameLIGO -> "cameligo"
  | ReasonLIGO -> "reasonligo"
  | JsLIGO -> "jsligo"

(* Preprocessing *)

type options = Compiler_options.t

let preprocess_file ~raise ~(options:options) ~meta file_path
  : Preprocessing.Pascaligo.success =
  let open Preprocessing in
  let preprocess_file =
    match meta.syntax with
      PascaLIGO  -> Pascaligo.preprocess_file
    | CameLIGO   -> Cameligo.preprocess_file
    | ReasonLIGO -> Reasonligo.preprocess_file
    | JsLIGO     -> Jsligo.preprocess_file
  in trace ~raise preproc_tracer @@
      Trace.from_result (preprocess_file options.libs file_path)

let preprocess_string ~raise ~(options:options) ~meta file_path =
  let open Preprocessing in
  let preprocess_string =
    match meta.syntax with
      PascaLIGO  -> Pascaligo.preprocess_string
    | CameLIGO   -> Cameligo.preprocess_string
    | ReasonLIGO -> Reasonligo.preprocess_string
    | JsLIGO     -> Jsligo.preprocess_string
  in trace ~raise preproc_tracer @@
     from_result (preprocess_string options.libs file_path)

(* Front-end compilation *)

type file_path = string

let parse_and_abstract_pascaligo ~raise ~add_warning buffer file_path =
  let add_warning w = add_warning @@ Main_warnings.self_cst_pascaligo_warning_tracer w in
  let raw =
    trace ~raise parser_tracer @@
    Parsing.Pascaligo.parse_file buffer file_path in
  let applied =
    trace ~raise self_cst_pascaligo_tracer @@
    Self_cst.Pascaligo.all_module ~add_warning raw in
  let imperative =
    trace ~raise cit_pascaligo_tracer @@
    Tree_abstraction.Pascaligo.compile_module applied
  in imperative

let parse_and_abstract_expression_pascaligo ~raise ~add_warning buffer =
  let add_warning w = add_warning @@ Main_warnings.self_cst_pascaligo_warning_tracer w in
  let raw =
    trace ~raise parser_tracer @@
    Parsing.Pascaligo.parse_expression buffer in
  let applied =
    trace ~raise self_cst_pascaligo_tracer @@
    Self_cst.Pascaligo.all_expression ~add_warning raw in
  let imperative =
    trace ~raise cit_pascaligo_tracer @@
    Tree_abstraction.Pascaligo.compile_expression applied
  in imperative

let parse_and_abstract_cameligo ~raise ~add_warning buffer file_path =
  let add_warning_ w = add_warning @@ Main_warnings.self_cst_cameligo_warning_tracer w in
  let raw =
    trace ~raise parser_tracer @@
    Parsing.Cameligo.parse_file buffer file_path in
  let applied =
    trace ~raise self_cst_cameligo_tracer @@
    Self_cst.Cameligo.all_module ~add_warning:add_warning_ raw in
  let add_warning_ w = add_warning @@ Main_warnings.cit_cameligo_warning_tracer w in
  let imperative =
    trace ~raise cit_cameligo_tracer @@
    Tree_abstraction.Cameligo.compile_module ~add_warning:add_warning_ applied
  in imperative

let parse_and_abstract_expression_cameligo ~raise ~add_warning buffer =
  let add_warning_ w = add_warning @@ Main_warnings.self_cst_cameligo_warning_tracer w in
  let raw =
    trace ~raise parser_tracer @@
    Parsing.Cameligo.parse_expression buffer in
  let applied =
    trace ~raise self_cst_cameligo_tracer @@
    Self_cst.Cameligo.all_expression ~add_warning:add_warning_ raw in
  let add_warning_ w = add_warning @@ Main_warnings.cit_cameligo_warning_tracer w in
  let imperative =
    trace ~raise cit_cameligo_tracer @@
    Tree_abstraction.Cameligo.compile_expression ~add_warning:add_warning_ applied
  in imperative

let parse_and_abstract_reasonligo ~raise ~add_warning buffer file_path =
  let add_warning w = add_warning @@ Main_warnings.self_cst_reasonligo_warning_tracer w in
  let raw =
    trace ~raise parser_tracer @@
    Parsing.Reasonligo.parse_file buffer file_path in
  let applied =
    trace ~raise self_cst_reasonligo_tracer @@
    Self_cst.Reasonligo.all_module ~add_warning raw in
  let imperative =
    trace ~raise cit_reasonligo_tracer @@
    Tree_abstraction.Reasonligo.compile_module applied
  in imperative

let parse_and_abstract_expression_reasonligo ~raise ~add_warning buffer =
  let add_warning w = add_warning @@ Main_warnings.self_cst_reasonligo_warning_tracer w in
  let raw =
    trace ~raise parser_tracer @@
    Parsing.Reasonligo.parse_expression buffer in
  let applied =
    trace ~raise self_cst_reasonligo_tracer @@
    Self_cst.Reasonligo.all_expression ~add_warning raw in
  let imperative =
    trace ~raise cit_reasonligo_tracer @@
    Tree_abstraction.Reasonligo.compile_expression applied
  in imperative

let parse_and_abstract_jsligo ~raise ~add_warning buffer file_path =
  let add_warning w = add_warning @@ Main_warnings.self_cst_jsligo_warning_tracer w in
  let raw =
    trace ~raise parser_tracer @@
    Parsing.Jsligo.parse_file buffer file_path in
  let applied =
    trace ~raise self_cst_jsligo_tracer @@
    Self_cst.Jsligo.all_module ~add_warning raw in
  let imperative =
    trace ~raise cit_jsligo_tracer @@
    Tree_abstraction.Jsligo.compile_module applied
  in imperative

let parse_and_abstract_expression_jsligo ~raise ~add_warning buffer =
  let add_warning w = add_warning @@ Main_warnings.self_cst_jsligo_warning_tracer w in
  let raw =
    trace ~raise parser_tracer @@
    Parsing.Jsligo.parse_expression buffer in
  let applied =
    trace ~raise self_cst_jsligo_tracer @@
    Self_cst.Jsligo.all_expression ~add_warning raw in
  let imperative =
    trace ~raise cit_jsligo_tracer @@
    Tree_abstraction.Jsligo.compile_expression applied
  in imperative

let parse_and_abstract ~raise ~meta ~add_warning buffer file_path
    : Ast_imperative.module_ =
  let parse_and_abstract =
    match meta.syntax with
      PascaLIGO  -> parse_and_abstract_pascaligo ~add_warning
    | CameLIGO   -> parse_and_abstract_cameligo ~add_warning
    | ReasonLIGO -> parse_and_abstract_reasonligo ~add_warning
    | JsLIGO     -> parse_and_abstract_jsligo ~add_warning in
  let abstracted =
    parse_and_abstract ~raise buffer file_path in
  let applied =
    trace ~raise self_ast_imperative_tracer @@
    Self_ast_imperative.all_module abstracted ~add_warning in
  applied

let parse_and_abstract_expression ~raise ~meta ~add_warning buffer =
  let parse_and_abstract =
    match meta.syntax with
      PascaLIGO ->
        parse_and_abstract_expression_pascaligo ~add_warning
    | CameLIGO ->
        parse_and_abstract_expression_cameligo ~add_warning
    | ReasonLIGO ->
        parse_and_abstract_expression_reasonligo ~add_warning
    | JsLIGO ->
        parse_and_abstract_expression_jsligo ~add_warning
      in
  let abstracted =
    parse_and_abstract ~raise buffer in
  let applied =
    trace ~raise self_ast_imperative_tracer @@
    Self_ast_imperative.all_expression abstracted
  in applied

let parse_and_abstract_string_reasonligo ~raise buffer =
  let raw = trace ~raise parser_tracer @@
    Parsing.Reasonligo.parse_string buffer in
  let imperative = trace ~raise cit_reasonligo_tracer @@
    Tree_abstraction.Reasonligo.compile_module raw
  in imperative

let parse_and_abstract_string_pascaligo ~raise buffer =
  let raw =
    trace ~raise parser_tracer @@
    Parsing.Pascaligo.parse_string buffer in
  let imperative =
    trace ~raise cit_pascaligo_tracer @@
    Tree_abstraction.Pascaligo.compile_module raw
  in imperative

let parse_and_abstract_string_cameligo ~raise ~add_warning buffer =
  let raw =
    trace ~raise parser_tracer @@
    Parsing.Cameligo.parse_string buffer in
  let imperative =
    trace ~raise cit_cameligo_tracer @@
    Tree_abstraction.Cameligo.compile_module ~add_warning raw
  in imperative

let parse_and_abstract_string_jsligo ~raise buffer =
  let raw =
    trace ~raise parser_tracer @@
    Parsing.Jsligo.parse_string buffer in
  let imperative =
    trace ~raise cit_jsligo_tracer @@
    Tree_abstraction.Jsligo.compile_module raw
  in imperative

let parse_and_abstract_string ~raise ~add_warning syntax buffer =
  let parse_and_abstract =
    match syntax with
      PascaLIGO ->
        parse_and_abstract_string_pascaligo
    | CameLIGO ->
      let add_warning w = add_warning @@ Main_warnings.cit_cameligo_warning_tracer w in
        parse_and_abstract_string_cameligo ~add_warning
    | ReasonLIGO ->
        parse_and_abstract_string_reasonligo
    | JsLIGO ->
        parse_and_abstract_string_jsligo in
  let abstracted =
    parse_and_abstract ~raise buffer in
  let applied =
    trace ~raise self_ast_imperative_tracer @@
    Self_ast_imperative.all_module abstracted ~add_warning
  in applied

let pretty_print_pascaligo_cst =
  Parsing.Pascaligo.pretty_print_cst

let pretty_print_cameligo_cst =
  Parsing.Cameligo.pretty_print_cst

let pretty_print_reasonligo_cst =
  Parsing.Reasonligo.pretty_print_cst

let pretty_print_jsligo_cst =
  Parsing.Jsligo.pretty_print_cst

let pretty_print_cst ~raise ~meta buffer file_path=
  let print =
    match meta.syntax with
      PascaLIGO  -> pretty_print_pascaligo_cst
    | CameLIGO   -> pretty_print_cameligo_cst
    | ReasonLIGO -> pretty_print_reasonligo_cst
    | JsLIGO     -> pretty_print_jsligo_cst
  in trace ~raise parser_tracer @@ print buffer file_path

let pretty_print_pascaligo =
  Parsing.Pascaligo.pretty_print_file

let pretty_print_cameligo =
  Parsing.Cameligo.pretty_print_file

let pretty_print_reasonligo =
  Parsing.Reasonligo.pretty_print_file

let pretty_print_jsligo =
  Parsing.Jsligo.pretty_print_file

let pretty_print ~raise ~meta buffer file_path =
  let print =
    match meta.syntax with
      PascaLIGO  -> pretty_print_pascaligo
    | CameLIGO   -> pretty_print_cameligo
    | ReasonLIGO -> pretty_print_reasonligo
    | JsLIGO     -> pretty_print_jsligo
  in trace ~raise parser_tracer @@ print buffer file_path
