(* Driver for the CameLIGO parser *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* Internal dependencies *)

module Comments      = Preprocessing_cameligo.Comments
module File          = Preprocessing_cameligo.File
module Token         = Lexing_cameligo.Token
module Self_tokens   = Lexing_cameligo.Self_tokens
module CST           = Cst.Cameligo
module ParErr        = Parser_msg
module ParserMainGen = Parsing_shared.ParserMainGen

(* CLIs *)

module Preproc_CLI = Preprocessor.CLI.Make (Comments)
module   Lexer_CLI =     LexerLib.CLI.Make (Preproc_CLI)
module  Parser_CLI =    ParserLib.CLI.Make (Lexer_CLI)

(* Renamings on the parser generated by Menhir to suit the functor. *)

module Parser =
  struct
    include Parsing_cameligo.Parser
    type tree = CST.t

    let main = contract

    module Incremental =
      struct
        let main = Incremental.contract
      end
  end

module Pretty =
  struct
    include Parsing_cameligo.Pretty
    type tree = CST.t
  end

module PrintTokens =
  struct
    include Cst_cameligo.PrintTokens
    type tree = CST.t
  end

module PrintCST =
  struct
    include Cst_cameligo.PrintCST
    type tree = CST.t
  end

(* Finally... *)

module Main = ParserMainGen.Make
                (File)
                (Comments)
                (Token)
                (Self_tokens)
                (CST)
                (ParErr)
                (Parser)
                (PrintTokens)
                (PrintCST)
                (Pretty)
                (Parser_CLI)

let () = Main.check_cli ()
let () = Main.parse ()
