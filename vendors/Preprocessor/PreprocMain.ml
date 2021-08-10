(* Standalone preprocessor with default settings *)

module CLI = Preprocessor.CLI
module PreprocMainGen = Preprocessor.PreprocMainGen

module Config =
  struct
    type block_comment_delimiters = <opening : string; closing : string>
    type line_comment_delimiter   = string (* Opening of a line comment *)
    type string_delimiter         = string

    let block    = None
    let line     = None
    let string   = Some "\""
    let file_ext = None
  end

module Parameters = CLI.Make (Config)
module Main = PreprocMainGen.Make (Parameters)

let () = Main.check_cli ()
let () = Main.preprocess () |> ignore
