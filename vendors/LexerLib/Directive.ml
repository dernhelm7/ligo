(* Definition of preprocessing directives for the lexer *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* Directives *)

type linenum    = int
type file_path  = string
type flag       = Push | Pop
type linemarker = linenum * file_path * flag option

type t =
  Linemarker of linemarker Region.reg

type directive = t

(* Printing *)

type lexeme = string

let sprintf = Printf.sprintf

let to_lexeme = function
  Linemarker Region.{value; _} ->
    let linenum, file_path, flag = value in
    let flag_lex =
      match flag with
        Some Push -> " 1"
      | Some Pop -> " 0"
      | None -> ""
    in sprintf "#%d %S%s" linenum file_path flag_lex

let project = function
  Linemarker Region.{value; region} ->
    let linenum, file_path, flag = value in
    let flag_str =
      match flag with
        Some Push -> "Some Push"
      | Some Pop -> "Some Pop"
      | None -> "None" in
    let value =
      sprintf "LineMarker (%d, %S, %s)" linenum file_path flag_str
    in Region.{region; value}

let to_string ~offsets mode directive =
  let Region.{region; value} = project directive in
  let reg_str = region#compact ~offsets mode
  in sprintf "%s: %s" reg_str value

let to_region d = (project d).region
