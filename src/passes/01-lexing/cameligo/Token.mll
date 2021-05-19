(* ocamllex specification for CameLIGO *)
{
(* START HEADER *)

(* Vendor dependencies *)

module Region    = Simple_utils.Region
module Markup    = LexerLib.Markup
module Directive = LexerLib.Directive

(* Utility modules *)

module SMap = Map.Make (String)
module SSet = Set.Make (String)

(* TOKENS *)

type lexeme = string

module T =
  struct
    type t =
      (* Preprocessing directives *)

      Directive of Directive.t

      (* Literals *)

    | String   of lexeme Region.reg
    | Verbatim of lexeme Region.reg
    | Bytes    of (lexeme * Hex.t) Region.reg
    | Int      of (lexeme * Z.t) Region.reg
    | Nat      of (lexeme * Z.t) Region.reg
    | Mutez    of (lexeme * Z.t) Region.reg
    | Ident    of lexeme Region.reg
    | UIdent   of lexeme Region.reg
    | Lang     of lexeme Region.reg Region.reg
    | Attr     of string Region.reg

    (* Symbols *)

    | ARROW    of Region.t  (* "->" *)
    | CONS     of Region.t  (* "::" *)
    | CARET    of Region.t  (* "^"  *)
    | MINUS    of Region.t  (* "-"  *)
    | PLUS     of Region.t  (* "+"  *)
    | SLASH    of Region.t  (* "/"  *)
    | TIMES    of Region.t  (* "*"  *)
    | LPAR     of Region.t  (* "("  *)
    | RPAR     of Region.t  (* ")"  *)
    | LBRACKET of Region.t  (* "["  *)
    | RBRACKET of Region.t  (* "]"  *)
    | LBRACE   of Region.t  (* "{"  *)
    | RBRACE   of Region.t  (* "}"  *)
    | COMMA    of Region.t  (* ","  *)
    | SEMI     of Region.t  (* ";"  *)
    | VBAR     of Region.t  (* "|"  *)
    | COLON    of Region.t  (* ":"  *)
    | DOT      of Region.t  (* "."  *)
    | WILD     of Region.t  (* "_"  *)
    | EQ       of Region.t  (* "="  *)
    | NE       of Region.t  (* "<>" *)
    | LT       of Region.t  (* "<"  *)
    | GT       of Region.t  (* ">"  *)
    | LE       of Region.t  (* "<=" *)
    | GE       of Region.t  (* ">=" *)
    | BOOL_OR  of Region.t  (* "||" *)
    | BOOL_AND of Region.t  (* "&&" *)

    (* Keywords *)

    | Begin     of Region.t  (* begin  *)
    | Else      of Region.t  (* else   *)
    | End       of Region.t  (* end    *)
    | False     of Region.t  (* false  *)
    | Fun       of Region.t  (* fun    *)
    | If        of Region.t  (* if     *)
    | In        of Region.t  (* in     *)
    | Let       of Region.t  (* let    *)
    | Match     of Region.t  (* match  *)
    | Mod       of Region.t  (* mod    *)
    | Module    of Region.t  (* module *)
    | Ctor_None of Region.t  (* None   *)
    | Not       of Region.t  (* not    *)
    | Of        of Region.t  (* of     *)
    | Or        of Region.t  (* or     *)
    | Rec       of Region.t  (* rec    *)
    | Ctor_Some of Region.t  (* Some   *)
    | Struct    of Region.t  (* struct *)
    | Then      of Region.t  (* then   *)
    | True      of Region.t  (* true   *)
    | Type      of Region.t  (* type   *)
    | With      of Region.t  (* with   *)

    (* End Of File *)

    | EOF of Region.t

    (* From tokens to lexemes *)

    let gen_sym prefix =
      let count = ref 0 in
      fun () -> incr count; prefix ^ string_of_int !count

    let id_sym  = gen_sym "id"
    and uid_sym = gen_sym "U"

    let concrete = function
      (* Identifiers, labels, numbers and strings *)

      "Ident"    -> id_sym ()
    | "UIdent"   -> uid_sym ()
    | "Int"      -> "1"
    | "Nat"      -> "1n"
    | "Mutez"    -> "1mutez"
    | "String"   -> "\"a string\""
    | "Verbatim" -> "{|verbatim|}"
    | "Bytes"    -> "0xAA"
    | "Attr"     -> "[@attr]"
    | "Lang"     -> "[%Michelson"

    (* Symbols *)

    | "ARROW" -> "->"
    | "CONS"  -> "::"
    | "CARET" -> "^"

    (* Arithmetics *)

    | "MINUS"   -> "-"
    | "PLUS"    -> "+"
    | "SLASH"   -> "/"
    | "TIMES"   -> "*"

    (* Compounds *)

    | "LPAR"     -> "("
    | "RPAR"     -> ")"
    | "LBRACKET" -> "["
    | "RBRACKET" -> "]"
    | "LBRACE"   -> "{"
    | "RBRACE"   -> "}"

    (* Separators *)

    | "COMMA" -> ","
    | "SEMI"  -> ";"
    | "VBAR"  -> "|"
    | "COLON" -> ":"
    | "DOT"   -> "."

    (* Wildcard *)

    | "WILD" -> "_"

    (* Comparisons *)

    | "EQ" -> "="
    | "NE" -> "<>"
    | "LT" -> "<"
    | "GT" -> ">"
    | "LE" -> "<="
    | "GE" -> ">="

    | "BOOL_OR"  -> "||"
    | "BOOL_AND" -> "&&"

    (* Keywords *)

    | "Begin"     -> "begin"
    | "Else"      -> "else"
    | "End"       -> "end"
    | "False"     -> "false"
    | "Fun"       -> "fun"
    | "If"        -> "if"
    | "In"        -> "in"
    | "Let"       -> "let"
    | "Match"     -> "match"
    | "Mod"       -> "mod"
    | "Module"    -> "module"
    | "Ctor_None" -> "None"
    | "Not"       -> "not"
    | "Of"        -> "of"
    | "Or"        -> "or"
    | "Rec"       -> "rec"
    | "Ctor_Some" -> "Some"
    | "Struct"    -> "struct"
    | "Then"      -> "then"
    | "True"      -> "true"
    | "Type"      -> "type"
    | "With"      -> "with"

    (* End Of File *)

    | "EOF" -> ""

    (* This case should not happen! *)

    | _  -> "\\Unknown" (* Backslash meant to trigger an error *)

    (* Projections *)

    let sprintf = Printf.sprintf

    type token = t

    let project = function
      (* Preprocessing directives *)

      Directive d ->
        let Region.{region; value} = Directive.project d
        in region, value

      (* Literals *)

    | String Region.{region; value} ->
        region, sprintf "String %S" value
    | Verbatim Region.{region; value} ->
        region, sprintf "Verbatim %S" value
    | Bytes Region.{region; value = s,b} ->
        region,
        sprintf "Bytes (%S, \"0x%s\")" s (Hex.show b)
    | Int Region.{region; value = s,n} ->
        region, sprintf "Int (%S, %s)" s (Z.to_string n)
    | Nat Region.{region; value = s,n} ->
        region, sprintf "Nat (%S, %s)" s (Z.to_string n)
    | Mutez Region.{region; value = s,n} ->
        region, sprintf "Mutez (%S, %s)" s (Z.to_string n)
    | Ident Region.{region; value} ->
        region, sprintf "Ident %S" value
    | UIdent Region.{region; value} ->
        region, sprintf "UIdent %S" value
    | Lang Region.{region; value} ->
        region, sprintf "Lang %S" (value.Region.value)
    | Attr Region.{region; value} ->
        region, sprintf "Attr %S" value

    (* Symbols *)

    | ARROW    region -> region, "ARROW"
    | CONS     region -> region, "CONS"
    | CARET    region -> region, "CARET"
    | MINUS    region -> region, "MINUS"
    | PLUS     region -> region, "PLUS"
    | SLASH    region -> region, "SLASH"
    | TIMES    region -> region, "TIMES"
    | LPAR     region -> region, "LPAR"
    | RPAR     region -> region, "RPAR"
    | LBRACKET region -> region, "LBRACKET"
    | RBRACKET region -> region, "RBRACKET"
    | LBRACE   region -> region, "LBRACE"
    | RBRACE   region -> region, "RBRACE"
    | COMMA    region -> region, "COMMA"
    | SEMI     region -> region, "SEMI"
    | VBAR     region -> region, "VBAR"
    | COLON    region -> region, "COLON"
    | DOT      region -> region, "DOT"
    | WILD     region -> region, "WILD"
    | EQ       region -> region, "EQ"
    | NE       region -> region, "NE"
    | LT       region -> region, "LT"
    | GT       region -> region, "GT"
    | LE       region -> region, "LE"
    | GE       region -> region, "GE"
    | BOOL_OR  region -> region, "BOOL_OR"
    | BOOL_AND region -> region, "BOOL_AND"

    (* Keywords *)

    | Begin     region -> region, "Begin"
    | Else      region -> region, "Else"
    | End       region -> region, "End"
    | False     region -> region, "False"
    | Fun       region -> region, "Fun"
    | If        region -> region, "If"
    | In        region -> region, "In"
    | Let       region -> region, "Let"
    | Match     region -> region, "Match"
    | Mod       region -> region, "Mod"
    | Module    region -> region, "Module"
    | Ctor_None region -> region, "Ctor_None"
    | Not       region -> region, "Not"
    | Of        region -> region, "Of"
    | Or        region -> region, "Or"
    | Rec       region -> region, "Rec"
    | Ctor_Some region -> region, "Ctor_Some"
    | Struct    region -> region, "Struct"
    | Then      region -> region, "Then"
    | True      region -> region, "True"
    | Type      region -> region, "Type"
    | With      region -> region, "With"

    (* End Of File *)

    | EOF region -> region, "EOF"

    (* From tokens to lexemes *)

    let to_lexeme = function
      (* Directives *)

      Directive d -> Directive.to_lexeme d

      (* Literals *)

    | String s   -> sprintf "%S" (String.escaped s.Region.value)
    | Verbatim v -> String.escaped v.Region.value
    | Bytes b    -> fst b.Region.value
    | Int i
    | Nat i
    | Mutez i    -> fst i.Region.value
    | Ident id   -> id.Region.value
    | UIdent id  -> id.Region.value
    | Attr a     -> sprintf "[@%s]" a.Region.value
    | Lang lang  -> Region.(lang.value.value)

    (* Symbols *)

    | ARROW    _ -> "->"
    | CONS     _ -> "::"
    | CARET    _ -> "^"
    | MINUS    _ -> "-"
    | PLUS     _ -> "+"
    | SLASH    _ -> "/"
    | TIMES    _ -> "*"
    | LPAR     _ -> "("
    | RPAR     _ -> ")"
    | LBRACKET _ -> "["
    | RBRACKET _ -> "]"
    | LBRACE   _ -> "{"
    | RBRACE   _ -> "}"
    | COMMA    _ -> ","
    | SEMI     _ -> ";"
    | VBAR     _ -> "|"
    | COLON    _ -> ":"
    | DOT      _ -> "."
    | WILD     _ -> "_"
    | EQ       _ -> "="
    | NE       _ -> "<>"
    | LT       _ -> "<"
    | GT       _ -> ">"
    | LE       _ -> "<="
    | GE       _ -> ">="
    | BOOL_OR  _ -> "||"
    | BOOL_AND  _ -> "&&"

    (* Keywords *)

    | Begin      _ -> "begin"
    | Else       _ -> "else"
    | End        _ -> "end"
    | False      _ -> "false"
    | Fun        _ -> "fun"
    | If         _ -> "if"
    | In         _ -> "in"
    | Let        _ -> "let"
    | Match      _ -> "match"
    | Mod        _ -> "mod"
    | Module     _ -> "module"
    | Ctor_None  _ -> "None"
    | Not        _ -> "not"
    | Of         _ -> "of"
    | Or         _ -> "or"
    | Rec        _ -> "rec"
    | Ctor_Some  _ -> "Some"
    | Struct     _ -> "struct"
    | True       _ -> "true"
    | Type       _ -> "type"
    | Then       _ -> "then"
    | With       _ -> "with"

    (* End Of File *)

    | EOF _ -> ""

    (* Converting a token to a string *)

    let to_string ~offsets mode token =
      let region, val_str = project token in
      let reg_str = region#compact ~offsets mode
      in sprintf "%s: %s" reg_str val_str

    (* Extracting the source region of a token *)

    let to_region token = project token |> fst

    (* Keywords *)

    let keywords = [
      (fun reg -> Begin     reg);
      (fun reg -> Else      reg);
      (fun reg -> End       reg);
      (fun reg -> False     reg); (* Data constructor *)
      (fun reg -> Fun       reg);
      (fun reg -> If        reg);
      (fun reg -> In        reg);
      (fun reg -> Let       reg);
      (fun reg -> Match     reg);
      (fun reg -> Mod       reg); (* Boolean operator *)
      (fun reg -> Module    reg);
      (fun reg -> Ctor_None reg); (* Data constructor *)
      (fun reg -> Not       reg); (* Boolean operator *)
      (fun reg -> Of        reg);
      (fun reg -> Or        reg); (* Boolean operator *)
      (fun reg -> Rec       reg);
      (fun reg -> Ctor_Some reg); (* Data constructor *)
      (fun reg -> Struct    reg);
      (fun reg -> Then      reg);
      (fun reg -> True      reg); (* Data constructor *)
      (fun reg -> Type      reg);
      (fun reg -> With      reg)
    ]

    (* Reserved identifiers *)

    let reserved = SSet.empty

    (* Making the lexicon up *)

    let add map (key, value) = SMap.add key value map

    let mk_map mk_key list =
      let apply map value = add map (mk_key value, value)
      in List.fold_left apply SMap.empty list

    type lexis = {
      kwd : (Region.t -> token) SMap.t;
      res : SSet.t
    }

    let lexicon : lexis =
      let build = mk_map (fun f -> to_lexeme (f Region.ghost))
      in {kwd = build keywords; res = reserved}

    (* Keywords *)

    type kwd_err = Invalid_keyword

    let mk_kwd ident region =
      match SMap.find_opt ident lexicon.kwd with
        Some mk_kwd -> Ok (mk_kwd region)
      |        None -> Error Invalid_keyword

    (* Identifiers *)

    type ident_err = Reserved_name

(* END OF HEADER *)
}

(* START LEXER DEFINITION *)

(* Named regular expressions *)

let small   = ['a'-'z']
let capital = ['A'-'Z']
let letter  = small | capital
let digit   = ['0'-'9']
let ident   = small (letter | '_' | digit)* |
              '_' (letter | '_' (letter | digit) | digit)+
let uident  = capital (letter | '_' | digit)*

(* Rules *)

rule scan_ident region lexicon = parse
  (ident as value) eof {
    if   SSet.mem value lexicon.res
    then Error Reserved_name
    else Ok (match SMap.find_opt value lexicon.kwd with
               Some mk_kwd -> mk_kwd region (* Ident which are keywords *)
             |        None -> Ident Region.{region; value}) }

and scan_uident region lexicon = parse
  (uident as value) eof {
    match SMap.find_opt value lexicon.kwd with
      Some mk_kwd -> mk_kwd region (* UIdent which are keywords *)
    |        None -> UIdent Region.{region; value} }

(* END LEXER DEFINITION *)

{
(* START TRAILER *)

    (* Smart constructors (injections) *)

    let mk_string lexeme region =
      String Region.{region; value=lexeme}

    let mk_verbatim lexeme region =
      Verbatim Region.{region; value=lexeme}

    let mk_bytes lexeme region =
      let norm = Str.(global_replace (regexp "_") "" lexeme) in
      let value = lexeme, `Hex norm
      in Bytes Region.{region; value}

    type int_err = Non_canonical_zero

    let mk_int lexeme region =
      let z =
        Str.(global_replace (regexp "_") "" lexeme) |> Z.of_string
      in if   Z.equal z Z.zero && lexeme <> "0"
         then Error Non_canonical_zero
         else Ok (Int Region.{region; value = lexeme,z})

    type nat_err =
      Invalid_natural
    | Non_canonical_zero_nat

    let mk_nat lexeme region =
      match String.index_opt lexeme 'n' with
        None -> Error Invalid_natural
      | Some _ ->
          let z =
            Str.(global_replace (regexp "_") "" lexeme) |>
              Str.(global_replace (regexp "n") "") |>
              Z.of_string in
          if   Z.equal z Z.zero && lexeme <> "0n"
          then Error Non_canonical_zero_nat
          else Ok (Nat Region.{region; value = lexeme,z})

    let mk_mutez lexeme region =
      let z = Str.(global_replace (regexp "_") "" lexeme) |>
                Str.(global_replace (regexp "mutez") "") |>
                Z.of_string in
      if   Z.equal z Z.zero && lexeme <> "0mutez"
      then Error Non_canonical_zero
      else Ok (Mutez Region.{region; value = lexeme, z})

    let eof region = EOF region

    type sym_err = Invalid_symbol of string

    let mk_sym lexeme region =
      match lexeme with
        (* Lexemes in common with all concrete syntaxes *)

        ";"   -> Ok (SEMI     region)
      | ","   -> Ok (COMMA    region)
      | "("   -> Ok (LPAR     region)
      | ")"   -> Ok (RPAR     region)
      | "["   -> Ok (LBRACKET region)
      | "]"   -> Ok (RBRACKET region)
      | "{"   -> Ok (LBRACE   region)
      | "}"   -> Ok (RBRACE   region)
      | "="   -> Ok (EQ       region)
      | ":"   -> Ok (COLON    region)
      | "|"   -> Ok (VBAR     region)
      | "."   -> Ok (DOT      region)
      | "_"   -> Ok (WILD     region)
      | "^"   -> Ok (CARET    region)
      | "+"   -> Ok (PLUS     region)
      | "-"   -> Ok (MINUS    region)
      | "*"   -> Ok (TIMES    region)
      | "/"   -> Ok (SLASH    region)
      | "<"   -> Ok (LT       region)
      | "<="  -> Ok (LE       region)
      | ">"   -> Ok (GT       region)
      | ">="  -> Ok (GE       region)

      (* Lexemes specific to CameLIGO *)

      | "->"  -> Ok (ARROW     region)
      | "<>"  -> Ok (NE        region)
      | "::"  -> Ok (CONS      region)
      | "||"  -> Ok (BOOL_OR   region)
      | "&&"  -> Ok (BOOL_AND  region)

      (* Invalid symbols *)

      | s ->  Error (Invalid_symbol s)

    (* Identifiers (starting with a smallcase letter) *)

    let mk_ident lexeme region =
      Lexing.from_string lexeme |> scan_ident region lexicon

    (* UIdent (starting with an uppercase letter) *)

    let mk_uident lexeme region =
      Lexing.from_string lexeme |> scan_uident region lexicon

    (* Attributes *)

    let mk_attr lexeme region = Attr Region.{value=lexeme; region}

    (* Code injection *)

    let mk_lang lang region = Lang Region.{value=lang; region}

    (* Verbatim string delimiters *)

    let verbatim_delimiters = "{|", "|}"

    (* Predicates *)

    let is_eof = function EOF _ -> true | _ -> false

    let is_string_delimiter s = (s = "\"")
  end

include T

module type S = module type of T

(* END TRAILER *)
}
