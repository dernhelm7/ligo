open Simple_utils.Display

type stacking_error = [
  | `Stacking_corner_case of string * string
  | `Stacking_contract_entrypoint of string
  | `Stacking_bad_iterator of Mini_c.constant'
  | `Stacking_could_not_tokenize_michelson of string
  | `Stacking_could_not_parse_michelson of string
  | `Stacking_untranspilable of Michelson.t * Michelson.t
]

let stage = "stacking"
let unstacking_stage = "unstacking_stage"
let corner_case_msg () = 
  "Sorry, we don't have a proper error message for this error. Please report \
   this use case so we can improve on this."

let corner_case ~loc  message = `Stacking_corner_case (loc,message)
let contract_entrypoint_must_be_literal ~loc = `Stacking_contract_entrypoint loc
let bad_iterator cst = `Stacking_bad_iterator cst
let unrecognized_data errs = `Stacking_unparsing_unrecognized_data errs
let untranspilable m_type m_data = `Stacking_untranspilable (m_type, m_data)
let bad_constant_arity c = `Stacking_bad_constant_arity c
let could_not_tokenize_michelson c = `Stacking_could_not_tokenize_michelson c
let could_not_parse_michelson c = `Stacking_could_not_parse_michelson c

let error_ppformat : display_format:string display_format ->
  stacking_error -> Location.t * string =
  fun ~display_format a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Stacking_corner_case (loc,msg) ->
      let s = Format.asprintf "Stacking corner case at %s : %s.\n%s"
        loc msg (corner_case_msg ()) in
      (Location.dummy, s);
    | `Stacking_contract_entrypoint loc ->
      let s = Format.asprintf "contract entrypoint must be given as a literal string: %s"
        loc in
      (Location.dummy, s);
    | `Stacking_bad_iterator cst ->
       let s = Format.asprintf "bad iterator: iter %a" Mini_c.PP.constant cst in
      (Location.dummy, s);
    | `Stacking_could_not_tokenize_michelson code ->
      (Location.dummy, Format.asprintf "Could not tokenize raw Michelson: %s" code)
    | `Stacking_could_not_parse_michelson code ->
      (Location.dummy, Format.asprintf "Could not parse raw Michelson: %s" code)
    | `Stacking_untranspilable (ty, value) ->
      (Location.dummy, Format.asprintf "Could not untranspile Michelson value: %a %a"
        Michelson.pp ty
        Michelson.pp value)
  )

let error_jsonformat : stacking_error -> Yojson.Safe.t = fun a ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
  | `Stacking_corner_case (loc,msg) ->
    let content = `Assoc [
      ("location", `String loc); 
      ("message", `String msg); ] in
    json_error ~stage ~content
  | `Stacking_contract_entrypoint loc ->
    let content = `Assoc [
      ("location", `String loc); 
      ("message", `String "contract entrypoint must be given as literal string"); ] in
    json_error ~stage ~content
  | `Stacking_bad_iterator cst ->
    let s = Format.asprintf "%a" Mini_c.PP.constant cst in
    let content = `Assoc [
       ("message", `String "bad iterator");
       ("iterator", `String s); ]
    in
    json_error ~stage ~content
  | `Stacking_could_not_tokenize_michelson code ->
    let content =
      `Assoc [("message", `String "Could not tokenize raw Michelson");
              ("code", `String code)] in
    json_error ~stage ~content
  | `Stacking_could_not_parse_michelson code ->
    let content =
      `Assoc [("message", `String "Could not parse raw Michelson");
              ("code", `String code)] in
    json_error ~stage ~content
  | `Stacking_untranspilable (ty, value) ->
    let content =
      `Assoc [("message", `String "Could not untranspile Michelson value");
              ("type", `String (Format.asprintf "%a" Michelson.pp ty));
              ("value", `String (Format.asprintf "%a" Michelson.pp value))] in
    json_error ~stage ~content
