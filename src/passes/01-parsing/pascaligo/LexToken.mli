(* This signature defines the lexical tokens for LIGO

   _Tokens_ are the abstract units which are used by the parser to
   build the abstract syntax tree (AST), in other words, the stream of
   tokens is the minimal model of the input program, carrying
   implicitly all its structure in a linear encoding, and nothing
   else, in particular, comments and whitespace are absent.

     A _lexeme_ is a specific character string (concrete
   representation) denoting a token (abstract representation). Tokens
   can be thought of as sets, and lexemes as elements of those sets --
   there is often an infinite number of lexemes, but a small number of
   tokens. (Think of identifiers as lexemes and one token.)

     The tokens are qualified here as being "lexical" because the
   parser generator Menhir expects to define them, in which context
   they are called "parsing tokens", and they are made to match each
   other. (This is an idiosyncratic terminology.)

     The type of the lexical tokens is the variant [t], also
   aliased to [token].
*)

(* Dependencies *)

module Region = Simple_utils.Region
module Pos    = Simple_utils.Pos
module Markup = Lexer_shared.Markup

(* TOKENS *)

type lexeme = string

type attribute = {
  header : string;
  string : lexeme Region.reg
}

type t =
  (* Literals *)

| Ident    of lexeme Region.reg
| Constr   of lexeme Region.reg
| Int      of (lexeme * Z.t) Region.reg
| Nat      of (lexeme * Z.t) Region.reg
| Mutez    of (lexeme * Z.t) Region.reg
| String   of lexeme Region.reg
| Verbatim of lexeme Region.reg
| Bytes    of (lexeme * Hex.t) Region.reg
| Attr     of string Region.reg
| Lang     of lexeme Region.reg Region.reg

  (* Symbols *)

| SEMI     of Region.t  (* ";"   *)
| COMMA    of Region.t  (* ","   *)
| LPAR     of Region.t  (* "("   *)
| RPAR     of Region.t  (* ")"   *)
| LBRACE   of Region.t  (* "{"   *)
| RBRACE   of Region.t  (* "}"   *)
| LBRACKET of Region.t  (* "["   *)
| RBRACKET of Region.t  (* "]"   *)
| CONS     of Region.t  (* "#"   *)
| VBAR     of Region.t  (* "|"   *)
| ARROW    of Region.t  (* "->"  *)
| ASS      of Region.t  (* ":="  *)
| EQ       of Region.t  (* "="   *)
| COLON    of Region.t  (* ":"   *)
| LT       of Region.t  (* "<"   *)
| LE       of Region.t  (* "<="  *)
| GT       of Region.t  (* ">"   *)
| GE       of Region.t  (* ">="  *)
| NE       of Region.t  (* "=/=" *)
| PLUS     of Region.t  (* "+"   *)
| MINUS    of Region.t  (* "-"   *)
| SLASH    of Region.t  (* "/"   *)
| TIMES    of Region.t  (* "*"   *)
| DOT      of Region.t  (* "."   *)
| WILD     of Region.t  (* "_"   *)
| CAT      of Region.t  (* "^"   *)

  (* Keywords *)

| And        of Region.t  (* "and"        *)
| Begin      of Region.t  (* "begin"      *)
| BigMap     of Region.t  (* "big_map"    *)
| Block      of Region.t  (* "block"      *)
| Case       of Region.t  (* "case"       *)
| Const      of Region.t  (* "const"      *)
| Contains   of Region.t  (* "contains"   *)
| Else       of Region.t  (* "else"       *)
| End        of Region.t  (* "end"        *)
| False      of Region.t  (* "False"      *)
| For        of Region.t  (* "for"        *)
| From       of Region.t  (* "from"       *)
| Function   of Region.t  (* "function"   *)
| Recursive  of Region.t  (* "recursive"  *)
| If         of Region.t  (* "if"         *)
| In         of Region.t  (* "in"         *)
| Is         of Region.t  (* "is"         *)
| List       of Region.t  (* "list"       *)
| Map        of Region.t  (* "map"        *)
| Mod        of Region.t  (* "mod"        *)
| Nil        of Region.t  (* "nil"        *)
| Not        of Region.t  (* "not"        *)
| Of         of Region.t  (* "of"         *)
| Or         of Region.t  (* "or"         *)
| Patch      of Region.t  (* "patch"      *)
| Record     of Region.t  (* "record"     *)
| Remove     of Region.t  (* "remove"     *)
| Set        of Region.t  (* "set"        *)
| Skip       of Region.t  (* "skip"       *)
| Step       of Region.t  (* "step"       *)
| Then       of Region.t  (* "then"       *)
| To         of Region.t  (* "to"         *)
| True       of Region.t  (* "True"       *)
| Type       of Region.t  (* "type"       *)
| Unit       of Region.t  (* "Unit"       *)
| Var        of Region.t  (* "var"        *)
| While      of Region.t  (* "while"      *)
| With       of Region.t  (* "with"       *)

  (* Data constructors *)

| C_None  of Region.t  (* "None"  *)
| C_Some  of Region.t  (* "Some"  *)

  (* Virtual tokens *)

| EOF of Region.t


type token = t

(* Projections

   The difference between extracting the lexeme and a string from a
   token is that the latter is the textual representation of the OCaml
   value denoting the token (its abstract syntax), rather than its
   lexeme (concrete syntax).
*)

val to_lexeme : token -> lexeme
val to_string : token -> ?offsets:bool -> [`Byte | `Point] -> string
val to_region : token -> Region.t

(* Injections *)

type   int_err = Non_canonical_zero
type ident_err = Reserved_name
type   nat_err = Invalid_natural
               | Non_canonical_zero_nat
type   sym_err = Invalid_symbol
type   kwd_err = Invalid_keyword

val mk_int      : lexeme -> Region.t -> (token,   int_err) result
val mk_nat      : lexeme -> Region.t -> (token,   nat_err) result
val mk_mutez    : lexeme -> Region.t -> (token,   int_err) result
val mk_ident    : lexeme -> Region.t -> (token, ident_err) result
val mk_sym      : lexeme -> Region.t -> (token,   sym_err) result
val mk_kwd      : lexeme -> Region.t -> (token,   kwd_err) result
val mk_string   : lexeme -> Region.t -> token
val mk_verbatim : lexeme -> Region.t -> token
val mk_bytes    : lexeme -> Region.t -> token
val mk_constr   : lexeme -> Region.t -> token
val mk_attr     : lexeme -> Region.t -> token
val mk_lang     : lexeme Region.reg -> Region.t -> token
val eof         : Region.t -> token

(* Predicates *)

val is_eof    : token -> bool

(* Style *)

type error

val error_to_string : error -> string

exception Error of error Region.reg

val format_error :
  ?offsets:bool -> [`Byte | `Point] ->
  error Region.reg -> file:bool -> string Region.reg

val check_right_context :
  token ->
  (Lexing.lexbuf -> (Markup.t list * token) option) ->
  Lexing.lexbuf ->
  unit

(* Unlexing the tokens: from strings containing the textual
   representation of tokens to lexemes. For example, [concrete "SEMI"
   = ";"]. *)

val concrete : string -> string
