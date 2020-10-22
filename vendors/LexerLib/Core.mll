(* A library for writing UTF8-aware lexers *)

{
(* START HEADER *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Pos    = Simple_utils.Pos
module FQueue = Simple_utils.FQueue

(* Rolling back one lexeme _within the current semantic action_ *)

let rollback buffer =
  let open Lexing in
  let len = String.length (lexeme buffer) in
  let pos_cnum = buffer.lex_curr_p.pos_cnum - len in
  buffer.lex_curr_pos <- buffer.lex_curr_pos - len;
  buffer.lex_curr_p <- {buffer.lex_curr_p with pos_cnum}

(* LEXER ENGINE *)

(* Resetting file name and line number in the lexing buffer

   The call [reset ~file ~line buffer] modifies in-place the lexing
   buffer [buffer] so the lexing engine records that the file
   associated with [buffer] is named [file], and the current line is
   [line]. *)

let reset_file file buffer =
  let open Lexing in
  buffer.lex_curr_p <- {buffer.lex_curr_p with pos_fname = file}

let reset_line line buffer =
  assert (line >= 0);
  let open Lexing in
  buffer.lex_curr_p <- {buffer.lex_curr_p with pos_lnum = line}

let reset_offset offset buffer =
  assert (offset >= 0);
  let open Lexing in
  let bol = buffer.lex_curr_p.pos_bol in
  buffer.lex_curr_p <- {buffer.lex_curr_p with pos_cnum = bol + offset }

let reset ?file ?(line=1) ?offset buffer =
  let () =
    match file with
      Some file -> reset_file file buffer
    |      None -> () in
  let () = reset_line line buffer in
  match offset with
    Some offset -> reset_offset offset buffer
  |        None -> ()

(* Utility types *)

type file_path = string
type lexeme = string

(* THREAD FOR STRUCTURED CONSTRUCTS (STRINGS, COMMENTS) *)

type thread = <
  opening     : Region.t;
  length      : int;
  acc         : char list;
  to_string   : string;
  push_char   : char -> thread;
  push_string : string -> thread;
  set_opening : Region.t -> thread
>

let mk_thread region : thread =
  (* The call [explode s a] is the list made by pushing the characters
     in the string [s] on top of [a], in reverse order. For example,
     [explode "ba" ['c';'d'] = ['a'; 'b'; 'c'; 'd']]. *)

  let explode s acc =
    let rec push = function
      0 -> acc
    | i -> s.[i-1] :: push (i-1)
    in push (String.length s) in
  object
    val opening = region
    method opening = opening

    val length = 0
    method length = length

    val acc = []
    method acc = acc

    method set_opening opening = {< opening; length; acc >}

    method push_char char =
      {< opening; length=length+1; acc=char::acc >}

    method push_string str =
      {< opening;
         length = length + String.length str;
         acc = explode str acc >}

    (* The value of [thread#to_string] is a string of length
       [thread#length] containing the characters in the list
       [thread#acc], in reverse order. For instance, [thread#to_string
       = "abc"] if [thread#length = 3] and [thread#acc =
       ['c';'b';'a']]. *)

    method to_string =
      let bytes = Bytes.make length ' ' in
      let rec fill i = function
        [] -> bytes
      | char::l -> Bytes.set bytes i char; fill (i-1) l
      in fill (length-1) acc |> Bytes.to_string
  end

(* STATE *)

(* Scanning the lexing buffer for tokens (and markup, as a
   side-effect).

     Because we want the lexer to have access to the right lexical
   context of a recognised lexeme (to enforce stylistic constraints or
   report special error patterns), we need to keep a hidden reference
   to a queue of recognised lexical units (that is, tokens and markup)
   that acts as a mutable state between the calls to [read]. When
   [read] is called, that queue is examined first and, if it contains
   at least one token, that token is returned; otherwise, the lexing
   buffer is scanned for at least one more new token. That is the
   general principle: we put a high-level buffer (our queue) on top of
   the low-level lexing buffer.

     One tricky and important detail is that we must make any parser
   generated by Menhir (and calling [read]) believe that the last
   region of the input source that was matched indeed corresponds to
   the returned token, despite that many tokens and markup may have
   been matched since it was actually read from the input. In other
   words, the parser requests a token that is taken from the
   high-level buffer, but the parser requests the source regions from
   the _low-level_ lexing buffer, and they may disagree if more than
   one token has actually been recognised.

     Consequently, in order to maintain a consistent view for the
   parser, we have to patch some fields of the lexing buffer, namely
   [lex_start_p] and [lex_curr_p], as these fields are read by parsers
   generated by Menhir when querying source positions (regions). This
   is the purpose of the function [patch_buffer]. After reading one or
   more tokens and markup by the scanning rule [scan], we have to save
   in the hidden reference [buf_reg] the region of the source that was
   matched by [scan]. This atomic sequence of patching, scanning and
   saving is implemented by the _function_ [scan] (beware: it shadows
   the scanning rule [scan]). The function [patch_buffer] is, of
   course, also called just before returning the token, so the parser
   has a view of the lexing buffer consistent with the token. *)

type 'token window = <
  last_token    : 'token option;
  current_token : 'token           (* Including EOF *)
>

type line_comment  = string (* Opening of a line comment *)
type block_comment = <opening : string; closing : string>

type command = [`Copy | `Units | `Tokens] option

type 'token config = <
  block     : block_comment option;
  line      : line_comment option;
  input     : file_path option;
  offsets   : bool;
  mode      : [`Byte | `Point];
  command   : command;
  is_eof    : 'token -> bool;
  to_region : 'token -> Region.t;
  to_lexeme : 'token -> string;
  to_string : offsets:bool -> [`Byte | `Point] -> 'token -> string
>

type 'token state = <
  config       : 'token config;

  units        : (Markup.t list * 'token) FQueue.t;
  markup       : Markup.t list;
  comments     : Markup.comment FQueue.t;
  window       : 'token window option;
  last         : Region.t;
  pos          : Pos.t;
  decoder      : Uutf.decoder;
  supply       : Bytes.t -> int -> int -> unit;

  enqueue      : 'token -> 'token state;
  set_units    : (Markup.t list * 'token) FQueue.t -> 'token state;
  set_last     : Region.t -> 'token state;
  set_pos      : Pos.t -> 'token state;
  slide_token  : 'token -> 'token state;

  sync         : Lexing.lexbuf -> 'token sync;

  push_newline : Lexing.lexbuf -> 'token state;
  push_line    : thread -> 'token state;
  push_block   : thread -> 'token state;
  push_space   : Lexing.lexbuf -> 'token state;
  push_tabs    : Lexing.lexbuf -> 'token state;
  push_bom     : Lexing.lexbuf -> 'token state;
  push_markup  : Markup.t -> 'token state;
  push_comment : Markup.comment -> 'token state
>

and 'token sync = {
  region : Region.t;
  lexeme : lexeme;
  state  : 'token state
}

type message = string Region.reg

type 'token scanner =
  'token state -> Lexing.lexbuf -> ('token state, message) Stdlib.result

type 'token cut = thread * 'token state -> 'token state

(* The type [client] gathers the arguments to the lexer in this
    module. *)

type 'token client = <
  mk_string : 'token cut;
  mk_eof    : 'token scanner;
  callback  : 'token scanner
>

type 'token internal_scanner =
  'token state -> Lexing.lexbuf -> 'token state

type 'token internal_client = <
  mk_string : 'token cut;
  mk_eof    : 'token internal_scanner;
  callback  : 'token internal_scanner
>

let mk_state ~config ~units ~markup ~comments
             ~window ~last ~pos ~decoder ~supply : 'token state =
  object (self)
    method config     = config
    val units         = units
    method units      = units
    val markup        = markup
    method markup     = markup
    val comments      = comments
    method comments   = comments
    val window        = window
    method window     = window
    val last          = last
    method last       = last
    val pos           = pos
    method pos        = pos
    method decoder    = decoder
    method supply     = supply

    method enqueue token =
      {< units = FQueue.enq (markup, token) units; markup=[] >}

    method set_units units  = {< units = units  >}
    method set_last  region = {< last  = region >}
    method set_pos   pos    = {< pos   = pos    >}

    method slide_token new_token =
      let new_window =
        match self#window with
          None -> object
                    method last_token    = None
                    method current_token = new_token
                  end
        | Some window ->
            object
              method last_token    = Some window#current_token
              method current_token = new_token
            end
      in {< window = Some new_window >}

    method sync buffer : 'token sync =
      let lexeme = Lexing.lexeme buffer in
      let length = String.length lexeme
      and start  = pos in
      let stop   = start#shift_bytes length in
      let state  = {< pos = stop >}
      and region = Region.make ~start:pos ~stop
      in {region; lexeme; state}

    (* MARKUP *)

    (* Committing markup to the current logical state *)

    method push_comment comment =
      {< comments = FQueue.enq comment comments >}

    method push_markup unit = {< markup = unit :: markup >}

    method push_newline buffer =
      let ()     = Lexing.new_line buffer in
      let value  = Lexing.lexeme buffer in
      let start  = self#pos in
      let stop   = start#new_line value in
      let region = Region.make ~start ~stop in
      let unit   = Markup.Newline Region.{region; value}
      in (self#push_markup unit)#set_pos stop

    method push_line thread =
      let start  = thread#opening#start in
      let region = Region.make ~start ~stop:self#pos
      and value  = thread#to_string in
      let reg    = Region.{region; value} in
      let unit   = Markup.LineCom reg
      in (self#push_markup unit)#push_comment (Markup.Line reg)

    method push_block thread =
      let start  = thread#opening#start in
      let region = Region.make ~start ~stop:self#pos
      and value  = thread#to_string in
      let reg    = Region.{region; value} in
      let unit   = Markup.BlockCom reg
      in (self#push_markup unit)#push_comment (Markup.Block reg)

    method push_space buffer =
      let {region; lexeme; state} = self#sync buffer in
      let value  = String.length lexeme in
      let unit   = Markup.Space Region.{region; value}
      in state#push_markup unit

    method push_tabs buffer =
      let {region; lexeme; state} = self#sync buffer in
      let value  = String.length lexeme in
      let unit   = Markup.Tabs Region.{region; value}
      in state#push_markup unit

    method push_bom buffer =
      let {region; lexeme; state} = self#sync buffer in
      let unit = Markup.BOM Region.{region; value=lexeme}
      in state#push_markup unit
  end

(* LEXER INSTANCE *)

(* input kind *)

type input =
  File    of file_path
| String  of string
| Channel of in_channel
| Buffer  of Lexing.lexbuf

(* Pretty-printing in a string the lexemes making up the markup
   between two tokens, concatenated with the last lexeme itself. *)

let output_token config out_channel (left_mark, token) =
  let output    str = Printf.fprintf out_channel "%s%!" str in
  let output_nl str = output (str ^ "\n")
  and offsets = config#offsets
  and mode    = config#mode in
  match config#command with
    Some `Tokens ->
      config#to_string ~offsets mode token |> output_nl
  | Some `Copy ->
      let lexeme = config#to_lexeme token
      and apply acc markup = Markup.to_lexeme markup :: acc
      in List.fold_left apply [lexeme] left_mark
         |> String.concat "" |> output
  | Some `Units ->
      let abs_token = config#to_string ~offsets mode token
      and apply acc markup =
        Markup.to_string ~offsets mode markup :: acc
      in List.fold_left apply [abs_token] left_mark
         |> String.concat "\n" |> output_nl
  | None -> ()

(* The lexer instance: the main exported data type *)

type 'token instance = {
  input        : input;
  read         : Lexing.lexbuf -> ('token, message) Stdlib.result;
  buffer       : Lexing.lexbuf;
  close        : unit -> unit;
  get_win      : unit -> 'token window option;
  get_pos      : unit -> Pos.t;
  get_last     : unit -> Region.t;
  get_file     : unit -> file_path;
  get_comments : unit -> Markup.comment FQueue.t
}

let lexbuf_from_input config = function
  String s ->
    Ok (Lexing.from_string s, fun () -> ())
| Channel chan ->
    let close () = close_in chan in
    Ok (Lexing.from_channel chan, close)
| Buffer b ->
    let () =
      match config#input with
        None | Some "" -> ()
        | Some path -> reset ~file:path b
    in Ok (b, fun () -> ())
| File "" ->
    Stdlib.Error (Region.wrap_ghost "File not found.")
| File path ->
    try
      let channel  = open_in path in
      let close () = close_in channel in
      let lexbuf   = Lexing.from_channel channel in
      let ()       = reset ~file:path lexbuf
      in Ok (lexbuf, close)
    with Sys_error msg -> Stdlib.Error (Region.wrap_ghost msg)

type 'token style_checker =
  'token config ->
  'token ->
  (Lexing.lexbuf -> (Markup.t list * 'token) option) ->
  Lexing.lexbuf ->
  (unit, message) Stdlib.result

(* Errors (NOT EXPORTED) *)

exception Error of string Region.reg

(* Encoding a function call in exception-raising style (ERS) to
   error-passing style (EPS) *)

let lift scanner lexbuf =
  try Stdlib.Ok (scanner lexbuf) with
    Error msg -> Stdlib.Error msg

(* Decoding a function call in EPS to ERS *)

let drop scanner lexbuf =
  match scanner lexbuf with
    Stdlib.Ok state -> state
  | Stdlib.Error msg -> raise (Error msg)

(* The main function *)

let open_token_stream config ~scan ~style input =
  let scan state = drop (scan state) in
  let file_path  = match config#input with
                     Some path -> path
                   | _ -> "" in
  let        pos = Pos.min ~file:file_path in
  let    buf_reg = ref (pos#byte, pos#byte)
  and    decoder = Uutf.decoder ~encoding:`UTF_8 `Manual in
  let     supply = Uutf.Manual.src decoder in
  let      state = ref (mk_state
                          ~config
                          ~units:FQueue.empty
                          ~last:Region.ghost
                          ~window:None
                          ~pos
                          ~markup:[]
                          ~comments:FQueue.empty
                          ~decoder
                          ~supply) in

  let get_pos      () = !state#pos
  and get_last     () = !state#last
  and get_win      () = !state#window
  and get_comments () = !state#comments
  and get_file     () = file_path in

  let patch_buffer (start, stop) buffer =
    let open Lexing in
    let file_path = buffer.lex_curr_p.pos_fname in
    buffer.lex_start_p <- {start with pos_fname = file_path};
    buffer.lex_curr_p  <- {stop  with pos_fname = file_path}

  and save_region buffer =
    buf_reg := Lexing.(buffer.lex_start_p, buffer.lex_curr_p) in

  let scan' scan buffer =
    patch_buffer !buf_reg buffer;
    state := scan !state buffer;
    save_region buffer in

  let next_token scan buffer =
    scan' scan buffer;
    match FQueue.peek !state#units with
      None -> None
    | Some (units, ext_token) ->
        state := !state#set_units units; Some ext_token in

  let rec read scan ~style ~log buffer =
    match FQueue.deq !state#units with
      None ->
        scan' scan buffer;
        read scan ~style ~log buffer
    | Some (units, (_, token as ext_token)) ->
        let style  = style config token (next_token scan)
        and region = config#to_region token in
        begin
          log ext_token;
          state := ((!state#set_units units)
                      #set_last region)
                     #slide_token token;
          drop style buffer;
          patch_buffer region#byte_pos buffer;
          token
        end in

  match lexbuf_from_input config input with
    Stdlib.Ok (buffer, close) ->
      let log = output_token config stdout in
      let read = lift (read scan ~style ~log) in
      let instance = {
          read; input; buffer; close; get_win;
          get_pos; get_last; get_file; get_comments}
      in Ok instance
  | Error _ as e -> e

(* LEXING COMMENTS AND STRINGS *)

(* Errors *)

type error =
  Invalid_utf8_sequence
| Unterminated_comment of string
| Unterminated_string
| Broken_string
| Invalid_character_in_string
| Undefined_escape_sequence

let sprintf = Printf.sprintf

let error_to_string = function
  Invalid_utf8_sequence ->
    "Invalid UTF-8 sequence."
| Undefined_escape_sequence ->
    "Undefined escape sequence.\n\
     Hint: Remove or replace the sequence."
| Unterminated_string ->
    "Unterminated string.\n\
     Hint: Close with double quotes."
| Unterminated_comment ending ->
    sprintf "Unterminated comment.\n\
             Hint: Close with %S." ending
| Broken_string ->
    "The string starting here is interrupted by a line break.\n\
     Hint: Remove the break, close the string before or insert a \
     backslash."
| Invalid_character_in_string ->
    "Invalid character in string.\n\
     Hint: Remove or replace the character."

let fail region error =
  let msg = error_to_string error in
  raise (Error Region.{value=msg;region})

(* Reading UTF-8 encoded characters *)

let scan_utf8_wrap scan_utf8 callback thread state lexbuf =
  let ()             = rollback lexbuf in
  let len            = thread#length in
  let thread, status = scan_utf8 thread state lexbuf in
  let delta          = thread#length - len in
  let stop           = state#pos#shift_one_uchar delta in
  match status with
    Stdlib.Ok () -> callback thread (state#set_pos stop) lexbuf
  | Stdlib.Error error ->
     let region = Region.make ~start:state#pos ~stop
     in fail region error

(* An input program may contain preprocessing directives, and the
   entry modules (named *Main.ml) run the preprocessor on them, as if
   using the GNU C preprocessor in traditional mode:

   https://gcc.gnu.org/onlinedocs/cpp/Traditional-Mode.html

     The main interest in using a preprocessor is that it can stand
   for a poor man's (flat) module system thanks to #include
   directives, and the equivalent of the traditional mode leaves the
   markup undisturbed.

     The line directives may carry some additional flags:

   https://gcc.gnu.org/onlinedocs/cpp/Preprocessor-Output.html

   of which 1 and 2 indicate, respectively, the start of a new file
   and the return from a file (after its inclusion has been
   processed). *)

let line_preproc scan_flags ~line ~file state lexbuf =
  let {state; _}   = state#sync lexbuf in
  let flags, state = scan_flags [] state lexbuf in
  let ()           = ignore flags
  and line         = int_of_string line in
  state#set_pos (state#pos#set ~file ~line ~offset:0)

(* END HEADER *)
}

(* START LEXER DEFINITION *)

(* NAMED REGULAR EXPRESSIONS *)

let utf8_bom   = "\xEF\xBB\xBF" (* Byte Order Mark for UTF-8 *)
let nl         = ['\n' '\r'] | "\r\n"
let blank      = ' ' | '\t'
let digit      = ['0'-'9']
let natural    = digit | digit (digit | '_')* digit
let string     = [^'"' '\\' '\n']*  (* For strings of #include *)
let hexa_digit = digit | ['A'-'F' 'a'-'f']
let byte       = hexa_digit hexa_digit
let esc        = "\\n" | "\\\"" | "\\\\" | "\\b"
               | "\\r" | "\\t" | "\\x" byte

(* Comment delimiters *)

let pascaligo_block_comment_opening = "(*"
let pascaligo_block_comment_closing = "*)"
let pascaligo_line_comment          = "//"

let cameligo_block_comment_opening = "(*"
let cameligo_block_comment_closing = "*)"
let cameligo_line_comment          = "//"

let reasonligo_block_comment_opening = "/*"
let reasonligo_block_comment_closing = "*/"
let reasonligo_line_comment          = "//"

let michelson_block_comment_opening = "/*"
let michelson_block_comment_closing = "*/"
let michelson_line_comment          = "#"

let block_comment_openings =
  pascaligo_block_comment_opening
| cameligo_block_comment_opening
| reasonligo_block_comment_opening
| michelson_block_comment_opening

let block_comment_closings =
  pascaligo_block_comment_closing
| cameligo_block_comment_closing
| reasonligo_block_comment_closing
| michelson_block_comment_closing

let line_comments =
  pascaligo_line_comment
| cameligo_line_comment
| reasonligo_line_comment
| michelson_line_comment

(* SCANNERS *)

rule scan client state = parse
  (* Markup *)

  nl    { scan client (state#push_newline lexbuf) lexbuf }
| ' '+  { scan client (state#push_space   lexbuf) lexbuf }
| '\t'+ { scan client (state#push_tabs    lexbuf) lexbuf }

  (* Strings *)

| '"'  { let {region; state; _} = state#sync lexbuf in
         let thread             = mk_thread region
         in  scan_string thread state lexbuf |> client#mk_string }

  (* Comment *)

| block_comment_openings as lexeme {
    match state#config#block with
      Some block when block#opening = lexeme ->
        let {region; state; _} = state#sync lexbuf in
        let thread             = mk_thread region in
        let thread             = thread#push_string lexeme in
        let thread, state      = scan_block block thread state lexbuf in
        let state              = state#push_block thread
        in scan client state lexbuf
    | Some _ | None -> (* Not a comment for this syntax *)
        rollback lexbuf; client#callback state lexbuf }

| line_comments as lexeme {
    match state#config#line with
      Some line when line = lexeme ->
        let {region; state; _} = state#sync lexbuf in
        let thread             = mk_thread region in
        let thread             = thread#push_string lexeme in
        let thread, state      = scan_line thread state lexbuf in
        let state              = state#push_line thread
        in scan client state lexbuf
    | Some _ | None -> (* Not a comment for this syntax *)
        rollback lexbuf; client#callback state lexbuf }

  (* Line preprocessing directives (from #include) *)

| '#' blank* (natural as line) blank+ '"' (string as file) '"' {
    let state = line_preproc scan_flags ~line ~file state lexbuf
    in scan client state lexbuf }

  (* Other tokens *)

| eof { client#mk_eof state lexbuf }

| _ { rollback lexbuf;
      client#callback state lexbuf (* May raise exceptions *) }

(* Block comments

   (For Emacs: ("(*") The lexing of block comments must take care of
   embedded block comments that may occur within, as well as strings,
   so no substring "*/" or "*)" may inadvertently close the
   block. This is the purpose of the first case of the scanner
   [scan_block]. *)

and scan_block block thread state = parse
  '"' | block_comment_openings as lexeme {
    if   block#opening = lexeme || lexeme = "\""
    then let opening            = thread#opening in
         let {region; state; _} = state#sync lexbuf in
         let thread             = thread#push_string lexeme in
         let thread             = thread#set_opening region in
         let scan_next          = if   lexeme = "\""
                                  then scan_string
                                  else scan_block block in
         let thread, state      = scan_next thread state lexbuf in
         let thread             = thread#set_opening opening
         in scan_block block thread state lexbuf
    else begin
           rollback lexbuf;
           scan_char_in_block block thread state lexbuf
         end }

| block_comment_closings as lexeme {
    if   block#closing = lexeme
    then thread#push_string lexeme, (state#sync lexbuf).state
    else begin
           rollback lexbuf;
           scan_char_in_block block thread state lexbuf
         end }

| nl as nl {
    let ()     = Lexing.new_line lexbuf
    and state  = state#set_pos (state#pos#new_line nl)
    and thread = thread#push_string nl in
    scan_block block thread state lexbuf }

| eof { let err = Unterminated_comment block#closing
        in fail thread#opening err }

| _ { rollback lexbuf;
      scan_char_in_block block thread state lexbuf }

and scan_char_in_block block thread state = parse
  _ { let if_eof thread =
        let err = Unterminated_comment block#closing
        in fail thread#opening err in
      let scan_utf8 = scan_utf8_char if_eof
      and callback  = scan_block block in
      scan_utf8_wrap scan_utf8 callback thread state lexbuf }

(* Line comments *)

and scan_line thread state = parse
  nl as nl { let ()     = Lexing.new_line lexbuf
             and thread = thread#push_string nl
             and state  = state#set_pos (state#pos#new_line nl)
             in thread, state }
| eof      { thread, state }
| _        { let scan_utf8 = scan_utf8_char (fun _ -> Stdlib.Ok ())
             in scan_utf8_wrap scan_utf8 scan_line thread state lexbuf }

(* Scanning UTF-8 encoded characters *)

and scan_utf8_char if_eof thread state = parse
     eof { thread, if_eof thread }
| _ as c { let thread = thread#push_char c in
           let lexeme = Lexing.lexeme lexbuf in
           let () = state#supply (Bytes.of_string lexeme) 0 1 in
           match Uutf.decode state#decoder with
             `Uchar _     -> thread, Stdlib.Ok ()
           | `Malformed _
           | `End         -> thread, Stdlib.Error Invalid_utf8_sequence
           | `Await       -> scan_utf8_char if_eof thread state lexbuf }

(* Scanning strings *)

and scan_string thread state = parse
  nl     { fail thread#opening Broken_string }
| eof    { fail thread#opening Unterminated_string }
| ['\t' '\r' '\b']
         { let {region; _} = state#sync lexbuf
           in fail region Invalid_character_in_string }
| '"'    { let {state; _} = state#sync lexbuf
           in thread, state }
| esc    { let {lexeme; state; _} = state#sync lexbuf in
           let thread = thread#push_string lexeme
           in scan_string thread state lexbuf }
| '\\' _ { let {region; _} = state#sync lexbuf
           in fail region Undefined_escape_sequence }
| _ as c { let {state; _} = state#sync lexbuf in
           scan_string (thread#push_char c) state lexbuf }

(* Scanning the flags of the line preprocessing directives *)

and scan_flags acc state = parse
  blank+          { let {state; _} = state#sync lexbuf
                    in scan_flags acc state lexbuf                 }
| natural as code { let {state; _} = state#sync lexbuf
                    and acc = int_of_string code :: acc
                    in scan_flags acc state lexbuf                 }
| nl              { List.rev acc, state#set_pos (state#pos#add_nl) }
| eof             { List.rev acc, (state#sync lexbuf).state        }

(* Scanner called first *)

and init client state = parse
  utf8_bom { let state = state#push_bom lexbuf
             in scan client state lexbuf               }
| _        { rollback lexbuf; scan client state lexbuf }

(* END LEXER DEFINITION *)

{
(* START TRAILER *)

let mk_scan (client: 'token client) =
  let internal_client : 'token internal_client =
    let open Simple_utils.Utils in
    object
      method mk_string = client#mk_string
      method mk_eof    = drop <@ client#mk_eof
      method callback  = drop <@ client#callback
    end
  and first_call = ref true in
  fun state ->
    let scanner =
      if !first_call then (first_call := false; init) else scan
    in lift (scanner internal_client state)

let line_preproc ~line ~file state lexbuf =
  line_preproc scan_flags ~line ~file state lexbuf

(* END TRAILER *)
}