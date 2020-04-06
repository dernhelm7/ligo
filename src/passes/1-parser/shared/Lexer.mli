(* Lexer specification for LIGO, to be processed by [ocamllex].

   The underlying design principles are:

     (1) enforce stylistic constraints at a lexical level, in order to
         early reject potentially misleading or poorly written
         LIGO contracts;

     (2) provide precise error messages with hints as how to fix the
         issue, which is achieved by consulting the lexical
         right-context of lexemes;

     (3) be as independent as possible from the LIGO version, so
         upgrades have as little impact as possible on this
         specification: this is achieved by using the most general
         regular expressions to match the lexing buffer and broadly
         distinguish the syntactic categories, and then delegating a
         finer, second analysis to an external module making the
         tokens (hence a functor below);

     (4) support unit testing (lexing of the whole input with debug
         traces).

     A limitation to the independence with respect to the LIGO version
   lies in the errors that the external module building the tokens
   (which may be version-dependent) may have to report. Indeed these
   errors have to be contextualised by the lexer in terms of input
   source regions, so useful error messages can be printed, therefore
   they are part of the signature [TOKEN] that parameterises the
   functor generated here. For instance, if, in a future release of
   LIGO, new tokens are added, and the recognition of their lexemes
   entails new errors, the signature [TOKEN] will have to be augmented
   and this lexer specification changed. However, in practice, it is
   more likely that instructions or types will be added, instead of
   new kinds of tokens.
*)

module Region = Simple_utils.Region
module Pos = Simple_utils.Pos

type lexeme = string

(* TOKENS *)

(* The signature [TOKEN] exports an abstract type [token], so a lexer
   can be a functor over tokens. This enables to externalise
   version-dependent constraints in any module whose signature matches
   [TOKEN]. Generic functions to construct tokens are required.

   Note the predicate [is_eof], which caracterises the virtual token
   for end-of-file, because it requires special handling. Some of
   those functions may yield errors, which are defined as values of
   the type [int_err] etc. These errors can be better understood by
   reading the ocamllex specification for the lexer ([Lexer.mll]).
*)

module type TOKEN = sig
  type token

  (* Errors *)

  type int_err = Non_canonical_zero

  type ident_err = Reserved_name

  type nat_err = Invalid_natural | Non_canonical_zero_nat

  type sym_err = Invalid_symbol

  type attr_err = Invalid_attribute

  (* Injections *)

  val mk_int : lexeme -> Region.t -> (token, int_err) result

  val mk_nat : lexeme -> Region.t -> (token, nat_err) result

  val mk_mutez : lexeme -> Region.t -> (token, int_err) result

  val mk_ident : lexeme -> Region.t -> (token, ident_err) result

  val mk_sym : lexeme -> Region.t -> (token, sym_err) result

  val mk_string : lexeme -> Region.t -> token

  val mk_bytes : lexeme -> Region.t -> token

  val mk_constr : lexeme -> Region.t -> token

  val mk_attr : string -> lexeme -> Region.t -> (token, attr_err) result

  val eof : Region.t -> token

  (* Predicates *)

  val is_string : token -> bool

  val is_bytes : token -> bool

  val is_int : token -> bool

  val is_ident : token -> bool

  val is_kwd : token -> bool

  val is_constr : token -> bool

  val is_sym : token -> bool

  val is_eof : token -> bool

  (* Projections *)

  val to_lexeme : token -> lexeme

  val to_string : token -> ?offsets:bool -> [`Byte | `Point] -> string

  val to_region : token -> Region.t
end

(* The module type for lexers is [S]. It mainly exports the function
   [open_token_stream], which returns

     * a function [read] that extracts tokens from a lexing buffer,
       together with a lexing buffer [buffer] to read from,
     * a function [close] that closes that buffer,
     * a function [get_pos] that returns the current position, and
     * a function [get_last] that returns the region of the last
       recognised token.
     * a function [get_file] that returns the name of the file being scanned
       (empty string if [stdin]).

   Note that a module [Token] is exported too, because the signature
   of the exported functions depend on it.

   The call [read ~log] evaluates in a lexer (also known as a
   tokeniser or scanner) whose type is [Lexing.lexbuf -> token], and
   suitable for a parser generated by Menhir. The argument labelled
   [log] is a logger, that is, it may print a token and its left
   markup to a given channel, at the caller's discretion.
*)

module type S = sig
  module Token : TOKEN

  type token = Token.token

  type file_path = string

  type logger = Markup.t list -> token -> unit

  type window = Nil | One of token | Two of token * token

  val slide : token -> window -> window

  type instance = {
    read : log:logger -> Lexing.lexbuf -> token;
    buffer : Lexing.lexbuf;
    get_win : unit -> window;
    get_pos : unit -> Pos.t;
    get_last : unit -> Region.t;
    get_file : unit -> file_path;
    close : unit -> unit;
  }

  type input =
    | File of file_path (* "-" means stdin *)
    | Stdin
    | String of string
    | Channel of in_channel
    | Buffer of Lexing.lexbuf

  type open_err = File_opening of string

  val open_token_stream : input -> (instance, open_err) Stdlib.result

  (* Error reporting *)

  type error

  val error_to_string : error -> string

  exception Error of error Region.reg

  val format_error :
    ?offsets:bool ->
    [`Byte | `Point] ->
    error Region.reg ->
    file:bool ->
    string Region.reg
end

(* The functorised interface

   Note that the module parameter [Token] is re-exported as a
   submodule in [S].
*)

module Make (Token : TOKEN) : S with module Token = Token
