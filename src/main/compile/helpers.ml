open Trace
open Main_errors

type s_syntax = Syntax_name of string
type v_syntax = PascaLIGO | CameLIGO | ReasonLIGO

type source_type =
  | Expr_in_string of string
  | Expr_in_stdin

let syntax_to_variant (Syntax_name syntax) source =
  match syntax, source with
    "auto", Some sf ->
      (match Filename.extension sf with
         ".ligo" | ".pligo" -> ok PascaLIGO
       | ".mligo"           -> ok CameLIGO
       | ".religo"          -> ok ReasonLIGO
       | ext                -> fail (syntax_auto_detection ext))
  | ("pascaligo" | "PascaLIGO"),   _ -> ok PascaLIGO
  | ("cameligo" | "CameLIGO"),     _ -> ok CameLIGO
  | ("reasonligo" | "ReasonLIGO"), _ -> ok ReasonLIGO
  | _ -> fail (invalid_syntax syntax) 


let parse_and_abstract_pascaligo source =
  let%bind raw = trace parser_tracer @@
    Parser.Pascaligo.parse_file source in
  let%bind imperative = trace cit_pascaligo_tracer @@
    Tree_abstraction.Pascaligo.compile_program raw
  in ok imperative

let parse_and_abstract_expression_pascaligo source =
  let%bind raw = trace parser_tracer @@ match source with
    | Expr_in_string expr -> Parser.Pascaligo.parse_expression_string expr
    | Expr_in_stdin -> Parser.Pascaligo.parse_expression_stdin ()
  in
  let%bind imperative = trace cit_pascaligo_tracer @@
    Tree_abstraction.Pascaligo.compile_expression raw
  in ok imperative

let parse_and_abstract_cameligo source =
  let%bind raw = trace parser_tracer @@
    Parser.Cameligo.parse_file source in
  let%bind imperative = trace cit_cameligo_tracer @@
    Tree_abstraction.Cameligo.compile_program raw
  in ok imperative

let parse_and_abstract_expression_cameligo source =
  let%bind raw = trace parser_tracer @@ match source with
    | Expr_in_string expr -> Parser.Cameligo.parse_expression_string expr
    | Expr_in_stdin -> Parser.Cameligo.parse_expression_stdin ()
  in
  let%bind imperative = trace cit_cameligo_tracer @@
    Tree_abstraction.Cameligo.compile_expression raw
  in ok imperative

let parse_and_abstract_reasonligo source =
  let%bind raw = trace parser_tracer @@
    Parser.Reasonligo.parse_file source in
  let%bind imperative = trace cit_reasonligo_tracer @@
    Tree_abstraction.Reasonligo.compile_program raw
  in ok imperative

let parse_and_abstract_expression_reasonligo source =
  let%bind raw = trace parser_tracer @@ match source with
    | Expr_in_string expr -> Parser.Reasonligo.parse_expression_string expr
    | Expr_in_stdin -> Parser.Reasonligo.parse_expression_stdin ()
  in
  let%bind imperative = trace cit_reasonligo_tracer @@
    Tree_abstraction.Reasonligo.compile_expression raw
  in ok imperative

let parse_and_abstract syntax source : (Ast_imperative.program, _) Trace.result =
  let%bind parse_and_abstract =
    match syntax with
      PascaLIGO  -> ok parse_and_abstract_pascaligo
    | CameLIGO   -> ok parse_and_abstract_cameligo
    | ReasonLIGO -> ok parse_and_abstract_reasonligo in
  let%bind parsified = parse_and_abstract source in
  let%bind applied = trace self_ast_imperative_tracer @@
    Self_ast_imperative.all_program parsified in
  ok applied

let parse_and_abstract_expression syntax source =
  let%bind parse_and_abstract = match syntax with
    PascaLIGO  -> ok parse_and_abstract_expression_pascaligo
  | CameLIGO   -> ok parse_and_abstract_expression_cameligo
  | ReasonLIGO -> ok parse_and_abstract_expression_reasonligo in
  let%bind parsified = parse_and_abstract source in
  let%bind applied = trace self_ast_imperative_tracer @@
    Self_ast_imperative.all_expression parsified
  in ok applied

let parse_and_abstract_string_reasonligo source =
  let%bind raw = trace parser_tracer @@
    Parser.Reasonligo.parse_string source in
  let%bind imperative = trace cit_reasonligo_tracer @@
    Tree_abstraction.Reasonligo.compile_program raw
  in ok imperative

let parse_and_abstract_string_pascaligo source =
  let%bind raw = trace parser_tracer @@
    Parser.Pascaligo.parse_string source in
  let%bind imperative = trace cit_pascaligo_tracer @@
    Tree_abstraction.Pascaligo.compile_program raw
  in ok imperative

let parse_and_abstract_string_cameligo source =
  let%bind raw = trace parser_tracer @@
    Parser.Cameligo.parse_string source in
  let%bind imperative = trace cit_cameligo_tracer @@
    Tree_abstraction.Cameligo.compile_program raw
  in ok imperative

let parse_and_abstract_string syntax source =
  let%bind parse_and_abstract =
    match syntax with
      PascaLIGO  -> ok parse_and_abstract_string_pascaligo
    | CameLIGO   -> ok parse_and_abstract_string_cameligo
    | ReasonLIGO -> ok parse_and_abstract_string_reasonligo in
  let%bind parsified = parse_and_abstract source in
  let%bind applied = trace self_ast_imperative_tracer @@
    Self_ast_imperative.all_program parsified
  in ok applied

let pretty_print_pascaligo_cst source =
  let%bind ast = trace parser_tracer @@ Parser.Pascaligo.parse_file source in
  let buffer = Buffer.create 59 in
  let state =
    Cst_pascaligo.ParserLog.mk_state
      ~offsets:true
      ~mode:`Byte
      ~buffer in
  Cst_pascaligo.ParserLog.pp_cst state ast;
  ok buffer

let pretty_print_cameligo_cst source =
  let%bind ast = trace parser_tracer @@ Parser.Cameligo.parse_file source in
  let buffer = Buffer.create 59 in
  let state = (* TODO: Should flow from the CLI *)
    Cst_cameligo.ParserLog.mk_state
      ~offsets:true
      ~mode:`Point
      ~buffer in
  Cst_cameligo.ParserLog.pp_cst state ast;
  ok buffer

let pretty_print_reasonligo_cst source =
  let%bind ast = trace parser_tracer @@ Parser.Reasonligo.parse_file source in
  let buffer = Buffer.create 59 in
  let state = (* TODO: Should flow from the CLI *)
    Cst_reasonligo.ParserLog.mk_state
      ~offsets:true
      ~mode:`Point
      ~buffer in
  Cst_reasonligo.ParserLog.pp_cst state ast;
  ok buffer

let pretty_print_cst syntax source =
  let%bind v_syntax =
    syntax_to_variant syntax (Some source) in
  match v_syntax with
    PascaLIGO  -> pretty_print_pascaligo_cst  source
  | CameLIGO   -> pretty_print_cameligo_cst   source
  | ReasonLIGO -> pretty_print_reasonligo_cst source

let preprocess_pascaligo = Parser.Pascaligo.preprocess

let preprocess_cameligo = Parser.Cameligo.preprocess

let preprocess_reasonligo = Parser.Reasonligo.preprocess

let preprocess syntax source =
  let%bind v_syntax =
    syntax_to_variant syntax (Some source) in
  trace parser_tracer @@
  match v_syntax with
    PascaLIGO  -> preprocess_pascaligo  source
  | CameLIGO   -> preprocess_cameligo   source
  | ReasonLIGO -> preprocess_reasonligo source

let pretty_print_pascaligo source =
  let%bind ast = Parser.Pascaligo.parse_file source in
  let doc    = Parser_pascaligo.Pretty.print ast in
  let buffer = Buffer.create 131 in
  let width  =
    match Terminal_size.get_columns () with
      None -> 60
    | Some c -> c in
  let () = PPrint.ToBuffer.pretty 1.0 width buffer doc
  in Trace.ok buffer

let pretty_print_cameligo source =
  let%bind ast = Parser.Cameligo.parse_file source in
  let doc    = Parser_cameligo.Pretty.print ast in
  let buffer = Buffer.create 131 in
  let width  =
    match Terminal_size.get_columns () with
      None -> 60
    | Some c -> c in
  let () = PPrint.ToBuffer.pretty 1.0 width buffer doc
  in Trace.ok buffer

let pretty_print_reasonligo source =
  let%bind ast = Parser.Reasonligo.parse_file source in
  let doc    = Parser_reasonligo.Pretty.print ast in
  let buffer = Buffer.create 131 in
  let width  =
    match Terminal_size.get_columns () with
      None -> 60
    | Some c -> c in
  let () = PPrint.ToBuffer.pretty 1.0 width buffer doc
  in Trace.ok buffer

let pretty_print syntax source =
  let%bind v_syntax =
    syntax_to_variant syntax (Some source) in
  match v_syntax with
    PascaLIGO  -> trace parser_tracer @@ pretty_print_pascaligo  source
  | CameLIGO   -> trace parser_tracer @@ pretty_print_cameligo   source
  | ReasonLIGO -> trace parser_tracer @@ pretty_print_reasonligo source
