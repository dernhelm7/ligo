(* The API of the preprocessor

   In case of success, a buffer containing the preprocessed input is
   returned, together with the list of imported modules and their
   locations on the file system. In case of an error, we return the
   preprocessed buffer so far. *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* The functor *)

type file_path   = string
type module_name = string
type module_deps = (file_path * module_name) list
type success     = Buffer.t * module_deps

type message     = string Region.reg
type error       = Buffer.t option * message

type result      = (success, error) Stdlib.result
type 'src preprocessor = 'src -> result

module type S =
  sig
    (* Preprocessing from various sources *)

    val from_lexbuf  : Lexing.lexbuf preprocessor
    val from_channel : in_channel    preprocessor
    val from_string  : string        preprocessor
    val from_file    : file_path     preprocessor
    val from_buffer  : Buffer.t      preprocessor
  end

module Make (Config : Config.S) (Options : Options.S) : S
