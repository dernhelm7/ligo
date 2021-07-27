open Result

let (let*) = Result.bind

type t = {
  syntax_name:                string;
  file_types:                 string list;
  scope_name:                 string;
  folding_start_marker:       regexp option;
  folding_stop_marker:        regexp option;
  language_features:          language_features;
  syntax_patterns:            string list;
  repository:                 pattern list
}

and language_features = {
  operators: string list;
  string_delimiters: string list;
  comments: language_features_comments;
  brackets: (string * string) list;
  auto_closing_pairs: (string * string) list;
  surrounding_pairs: (string * string) list;
  syntax_table: (string * string) list; (* for Emacs *)
}

and language_features_comments = {
  line_comment:  string option;
  block_comment: (string * string) option
}

and highlight_name = 
  Comment
| Constant
| String
| Character
| Number
| Boolean
| Float
| Identifier
| Function
| Statement
| Conditional
| Repeat
| Label
| Operator
| Keyword
| Exception
| PreProc
| Type
| StorageClass
| Structure
| Typedef
| SpecialChar
| SpecialComment
| Underlined
| Error
| Todo

| Builtin_type
| Builtin_module
| Builtin_function

and error = 
  Referenced_rule_does_not_exist of string
| Meta_name_some_but_empty of string
| Begin_cant_be_empty of string
| End_cant_be_empty of string

and pattern = {
  name: string;
  kind: pattern_kind;
}

and regexp = string

and pattern_kind = 
  Begin_end of begin_end_pattern
| Match     of match_pattern

and begin_end_pattern = {
  meta_name:      highlight_name option;
  begin_:         (regexp * highlight_name option) list;
  end_:           (regexp * highlight_name option) list;
  patterns:       string list;
}

and match_pattern = {
  match_:   regexp;
  match_name: highlight_name option;
  captures: (int * highlight_name) list
}

module JSON = struct 
 
  let highlight_to_textmate:string -> highlight_name -> string = fun syntax -> function
    Comment           -> "comment.block." ^ syntax
  | Constant          -> "constant.language." ^ syntax
  | String            -> "string.quoted.double." ^ syntax
  | Character         -> "constant.character." ^ syntax
  | Number            -> "constant.numeric." ^ syntax
  | Boolean           -> "constant.language." ^ syntax
  | Float             -> "constant.numeric." ^ syntax
  | Identifier        -> "storage.var." ^ syntax
  | Function          -> "keyword.other.let-binding." ^ syntax
  | Statement         -> "keyword.other." ^ syntax
  | Conditional       -> "keyword.control." ^ syntax
  | Repeat            -> "keyword.control." ^ syntax
  | Label             -> "variable.other.enummember." ^ syntax
  | Operator          -> "keyword.operator." ^ syntax
  | Keyword           -> "keyword.other." ^ syntax
  | Exception         -> "keyword.control." ^ syntax
  | PreProc           -> "meta.preprocessor." ^ syntax
  | Type              -> "storage.type." ^ syntax
  | StorageClass      -> "storage.modifier." ^ syntax
  | Structure         -> "storage.class." ^ syntax
  | Typedef           -> "storage.type." ^ syntax
  | SpecialChar       -> "constant.character." ^ syntax
  | SpecialComment    -> "comment.other." ^ syntax
  | Underlined        -> "markup.underline." ^ syntax
  | Error             -> "invalid.illegal." ^ syntax
  | Todo              -> "meta.todo." ^ syntax

  | Builtin_type      -> "support.type." ^ syntax
  | Builtin_module    -> "support.class." ^ syntax
  | Builtin_function  -> "support.function." ^ syntax 

  let rec capture syntax (i: int * highlight_name) = 
    (string_of_int (fst i), `Assoc [("name", `String (highlight_to_textmate syntax (snd i)))])

  and captures syntax (l: (int * highlight_name) list) = 
    `Assoc (List.map (capture syntax) l)

  and pattern_kind (syntax: string) = function 
    Begin_end { 
      meta_name;
      begin_; 
      end_; 
      patterns
    } -> 
      let (_, begin_captures) = 
        List.fold_left 
          (fun (n, result) c -> 
            let _, highlight_name = c in
            match highlight_name with 
              None -> (n + 1, result)
            | Some s -> (n + 1, (n, s) :: result)
          ) 
          (1, []) 
          begin_ 
      in
      let (_, end_captures) = 
        List.fold_left 
          (fun (n, result) c -> 
            let _, highlight_name = c in
            match highlight_name with 
              None -> (n + 1, result)
            | Some s -> (n + 1, (n, s) :: result)
          ) 
          (1, []) 
          end_ 
      in
      let begin_ = String.concat "" (List.map fst begin_) in
      let end_ = String.concat "" (List.map fst end_) in
      (match meta_name with 
        Some s -> [("name", `String (highlight_to_textmate syntax s))];
      | None -> [])
      @
      [
        ("begin", `String begin_);
        ("end", `String end_);
        ("beginCaptures", captures syntax begin_captures);
        ("endCaptures", captures syntax end_captures);
        ("patterns", `List (List.map (fun reference -> `Assoc [("include", `String ("#" ^ reference))]) patterns))
      ]
  | Match {match_; captures=c; match_name} -> 
    (match match_name with 
      Some s -> [("name", `String (highlight_to_textmate syntax s))]
    | None -> [])
    @
    [
      ("match", `String match_);      
      ("captures", captures syntax c)
    ] 
  
  and pattern syntax {name; kind} = 
    `Assoc ([
      ("name", `String name);
    ] @ pattern_kind syntax kind)

  and repository syntax r : Yojson.Safe.t = 
    `Assoc (List.map (fun i -> (i.name, `Assoc (pattern_kind syntax i.kind))) r)
    
  and language_features: language_features -> Yojson.Safe.t = fun l ->
    `Assoc (
      comments l.comments
      @ brackets "brackets" l.brackets
      @ brackets "autoClosingPairs" l.auto_closing_pairs
      @ brackets "surroundingPairs" l.surrounding_pairs)

    and comments: language_features_comments -> (string * Yojson.Safe.t) list = fun c ->
      [("comments", `Assoc (
      (match c.line_comment with 
          None -> []
        | Some s -> ["lineComment", `String s])
        @ 
        (match c.block_comment with 
          None -> []
        | Some (s, e) -> ["blockComment", `List [`String s; `String e]])
      ))]
    
    and brackets: string -> (string * string) list -> (string * Yojson.Safe.t) list = fun name l ->
      [(name, `List (List.map (fun b -> `List [`String (fst b); `String (snd b)]) l))]

  
  and to_yojson: string -> t -> (Yojson.Safe.t * Yojson.Safe.t, error) result = fun syntax s -> 
    ok @@ (`Assoc
    ((match s.folding_start_marker, s.folding_stop_marker with 
        Some folding_start_marker, Some folding_stop_marker -> [
          ("foldingStartMarker", `String folding_start_marker);
          ("foldingStopMarker", `String folding_stop_marker)
        ]
      | _, _ -> [])
    @
      [
      ("name", `String s.syntax_name);
      ("scopeName", `String s.scope_name);
      ("fileTypes", `List (List.map (fun s -> `String s) s.file_types));
      ("patterns", `List (List.map (fun reference -> `Assoc [("include", `String ("#" ^ reference))]) s.syntax_patterns));
      ("repository", repository syntax s.repository)
    ]),
    language_features s.language_features)

end

module Validate = struct

  let rec check_reference repository r =
    if r = "$self" then 
      ok true
    else 
      let exists = List.exists (fun i -> i.name = r) repository in 
      if exists then 
        ok true
      else 
        error (Referenced_rule_does_not_exist r)
        
  and pattern_kind name repository = function 
    Begin_end {begin_; end_; patterns; _} -> 
      let begin_ = String.concat "" (List.map fst begin_) in
      let end_ = String.concat "" (List.map fst end_) in
      if String.trim begin_ = "" then 
        error (Begin_cant_be_empty name)
      else if String.trim end_ = "" then
        error (End_cant_be_empty name)
      else
        let patterns = List.fold_left (fun a c -> if is_error a then a else (
          check_reference repository c)) (ok true) patterns in
        fold ~ok ~error patterns
  | Match _m -> ok true  
  
  let syntax s = 
    let repository = s.repository in
    let patterns = List.fold_left (fun a c -> if is_error a then a else pattern_kind c.name repository c.kind) (ok true) repository in
    let curr = fold ~ok ~error patterns in
    let patterns = List.fold_left (fun a c -> if is_error a then a else check_reference repository c) curr s.syntax_patterns in
    fold ~ok ~error patterns

end

let to_jsons: string -> t -> (Yojson.Safe.t * Yojson.Safe.t, error) result = fun syntax s ->
  let* _ = Validate.syntax s in
  JSON.to_yojson syntax s

module Helpers = struct 
  let macro = {
    name = "macro";
    kind = Begin_end {
      meta_name = Some String;
      begin_ = [("^\\s*((#)\\w+)", Some PreProc)];
      end_ = [("$", None)];
      patterns = [
        "string";
        "line_comment";
        "block_comment"
      ]
    }
  }

  let string = [
    {
      name = "string_specialchar";
      kind = Match {
        match_ = "\\\\.";
        match_name = Some SpecialChar;
        captures = []
      }
    };
    {
      name = "string";
      kind = Begin_end {
        meta_name = Some String;
        begin_ = [("\"", None)];
        end_ = [("\"", None)];
        patterns = [
          "string_specialchar"
        ]
      }
    }
  ]
    let ocaml_comment = [
      {
        name = "line_comment";
        kind = Match {
          match_name = Some Comment;
          match_ = "(//.*)";
          captures = []
        };
      };
      { name = "block_comment";
        kind = Begin_end {
          meta_name = Some Comment;
          begin_ = [("\\(\\*", None)];
          end_ = [("\\*\\)", None)];
          patterns = []
        }
      }
    ]

    let c_comment = [
      { name = "line_comment";
        kind = Match {
          match_name = Some Comment;
          match_ = "(//.*)";
          captures = []
        }
      };
      { name = "block_comment";
        kind = Begin_end {
          meta_name = Some Comment;
          begin_ = [("\\/\\*", None)];
          end_ = [("\\*\\/", None)];
          patterns = []
        }
      }
    ]

    let numeric_literals = {
      name = "numeric-literals";
      kind = Match {
        match_name = Some Number;
        match_ = "\\b\\d+";
        captures = []
      }
    }
  
end
