open Trace
open Helpers

let compile ?(libs=[]) (source_filename:string) syntax : (Ast_imperative.program , _) result =
  let%bind syntax = syntax_to_variant syntax (Some source_filename) in
  let%bind abstract = parse_and_abstract ~libs syntax source_filename in
  ok abstract

let compile_string ?(libs=[]) (source:string) syntax : (Ast_imperative.program , _) result =
  parse_and_abstract_string ~libs syntax source

let compile_expression : ?libs: string list -> v_syntax -> string -> (Ast_imperative.expression , _) result =
    fun ?(libs=[]) syntax exp ->
  parse_and_abstract_expression ~libs syntax exp

let compile_contract_input : ?libs: string list -> string -> string -> v_syntax -> (Ast_imperative.expression , _) result =
    fun ?(libs=[]) storage parameter syntax ->
  let%bind (storage,parameter) = bind_map_pair (compile_expression ~libs syntax) (storage,parameter) in
  ok @@ Ast_imperative.e_pair storage parameter

let pretty_print_cst source_filename syntax =
  Helpers.pretty_print_cst syntax source_filename

let preprocess source_filename syntax =
  Helpers.preprocess syntax source_filename

let pretty_print source_filename syntax =
  Helpers.pretty_print syntax source_filename
