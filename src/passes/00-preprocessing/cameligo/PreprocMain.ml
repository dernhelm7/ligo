(* Driving the standalone preprocessor for CameLIGO *)

module type CONFIG = Preprocessor.Config.S

module Config : CONFIG = Preprocessing_cameligo.Config

module Parameters = Preprocessor.CLI.Make (Config)
module Main = Preprocessor.PreprocMainGen.Make (Parameters)

let () =
  let open Main in
  match check_cli () with
    Main.Ok ->
      let {out; err}, _ = preprocess ()
      in Printf.printf  "%s%!" out;
         Printf.eprintf "%s%!" err
  | Info  msg -> Printf.printf "%s\n%!" msg
  | Error msg -> Printf.eprintf "%s\n%!" msg
