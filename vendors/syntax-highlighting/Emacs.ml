
type syntax_class = 
  White_space
| Word
| Symbol
| Open_parenthesis
| Close_parenthesis
| String_quote
| Escape_character
| Char_quote
| Paired_delimiters
| Expression_prefix
| Comment_starters
| Comment_enders
| Standard_syntax
| Generic_comment_delimiter
| Generic_string_delimiter

type color = 
  Orange
| Brown
| Khaki
| Dark_orange
| Black
| Wheat

type face_option = 
  Bold
| Background of color
| Foreground of color
| Inherit of string

type terminal = 
  Background_dark of face_option list
| Background_light of face_option list
| All of face_option list

type face = {
  name: string;
  terminals: terminal list;
  description: string;
}

module LigoFontLock = struct
  let character = {
    name = "ligo-font-lock-character-face";
    terminals = [
      All [Inherit "font-lock-string-face"]
    ];
    description = "Face description for characters."
  }
  let number = {
    name = "ligo-font-lock-number-face";
    terminals = [
      All [Inherit "default"]
    ];
    description = "Face description for numbers."
  }
  let float_ = {
    name = "ligo-font-lock-float-face";
    terminals = [
      All [Inherit "default"]
    ];
    description = "Face description for floats."
  }
  let builtin_function = {
    name = "ligo-font-lock-builtin-function-face";
    terminals = [
      All [Inherit "font-lock-function-name-face"]
    ];
    description = "Face description for builtin functions."
  }
  let statement = {
    name = "ligo-font-lock-statement-face";
    terminals = [
      All [Inherit "font-lock-keyword-face"]
    ];
    description = "Face description for statements."
  }
  let conditional = {
    name = "ligo-font-lock-conditional-face";
    terminals = [
      All [Inherit "font-lock-keyword-face"]
    ];
    description = "Face description for conditionals."
  }
  let repeat = {
    name = "ligo-font-lock-repeat-face";
    terminals = [
      All [Inherit "font-lock-keyword-face"]
    ];
    description = "Face description for repeat keywords."
  }
  let label = {
    name = "ligo-font-lock-label-face";
    terminals = [
      All [Inherit "default"]
    ];
    description = "Face description for labels."
  }
  let operator = {
    name = "ligo-font-lock-operator-face";
    terminals = [
      Background_light [
        Foreground Brown
      ];
      All [
        Foreground Khaki
      ]
    ];
    description = "Face description for operators."
  }
  let exception_ = {
    name = "ligo-font-lock-exception-face";
    terminals = [
      Background_light [
        Foreground Dark_orange 
      ];
      Background_dark [
        Foreground Orange
      ]
    ];
    description = "Face description for exceptions."
  }
  let builtin_type = {
    name = "ligo-font-lock-builtin-type-face";
    terminals = [
      All [Inherit "font-lock-type-face"]
    ];
    description = "Face description for builtin types."
  }
  let storage_class = {
    name = "ligo-font-lock-storage-class-face";
    terminals = [
      Background_light [
        Foreground Black;
        Bold
      ];
      All [
        Foreground Wheat;
        Bold
      ]
    ];
    description = "Face description for storage classes."
  }
  let builtin_module = {
    name = "ligo-font-lock-builtin-module-face";
    terminals = [
      All [
        Inherit "font-lock-function-name-face"
      ]
    ];
    description = "Face description for builtin modules."
  }
  let structure = {
    name = "ligo-font-lock-structure-face";
    terminals = [All [Inherit "default"]];
    description = "Face description for structures."
  }
  let type_def = {
    name = "ligo-font-lock-type-def-face";
    terminals = [
      All [Inherit "font-lock-type-face"]
    ];
    description = "Face description for type definitions."
  }
  let special_char = {
    name = "ligo-font-lock-special-char-face";
    terminals = [
      All [Inherit "font-lock-string-face"]
    ];
    description = "Face description for special characters."
  }
  let special_comment = {
    name = "ligo-font-lock-special-comment-face";
    terminals = [
      All [Inherit "font-lock-comment-face"]
    ];
    description = "Face description for special comments."
  }
  let error = {
    name = "ligo-font-lock-error-face";
    terminals = [
      All [Inherit "error"]
    ];
    description = "Face description for errors."
  }
  let todo = {
    name = "ligo-font-lock-todo-face";
    terminals = [
      All [Inherit "highlight"]
    ];
    description = "Face description for todos."
  }
end

let highlight_to_opt = function
    Textmate.Comment -> Some "font-lock-comment-face"
  | Constant         -> Some "font-lock-constant-face"
  | String           -> Some "font-lock-string-face"
  | Character        -> Some LigoFontLock.character.name
  | Number           -> Some LigoFontLock.number.name
  | Boolean          -> Some "font-lock-constant-face"
  | Float            -> Some LigoFontLock.float_.name
  | Identifier       -> Some "font-lock-variable-name-face" 
  | Builtin_function -> Some LigoFontLock.builtin_function.name
  | Function         -> Some "font-lock-function-name-face"
  | Statement        -> Some LigoFontLock.statement.name
  | Conditional      -> Some LigoFontLock.conditional.name
  | Repeat           -> Some LigoFontLock.repeat.name
  | Label            -> Some LigoFontLock.label.name
  | Operator         -> Some LigoFontLock.operator.name
  | Keyword          -> Some "font-lock-keyword-face"
  | Exception        -> Some LigoFontLock.exception_.name
  | PreProc          -> Some "font-lock-preprocessor-face"
  | Builtin_type     -> Some LigoFontLock.builtin_type.name
  | Type             -> Some "font-lock-type-face"
  | StorageClass     -> Some LigoFontLock.storage_class.name
  | Builtin_module   -> Some LigoFontLock.builtin_module.name
  | Structure        -> Some LigoFontLock.structure.name
  | Typedef          -> Some LigoFontLock.type_def.name
  | SpecialChar      -> Some LigoFontLock.special_char.name
  | SpecialComment   -> Some LigoFontLock.special_comment.name
  | Underlined       -> Some "underline"
  | Error            -> Some LigoFontLock.error.name
  | Todo             -> Some LigoFontLock.todo.name

module Print = struct
  open Format 

  let print_color fmt = function 
    Orange ->       fprintf fmt "\"%s\" " "orange"
  | Brown ->        fprintf fmt "\"%s\" " "brown"
  | Khaki ->        fprintf fmt "\"%s\" " "khaki"
  | Dark_orange ->  fprintf fmt "\"%s\" " "dark orange"
  | Black ->        fprintf fmt "\"%s\" " "black"
  | Wheat ->        fprintf fmt "\"%s\" " "wheat"

  let print_face_option: formatter -> face_option -> unit = fun fmt -> function
    Bold -> fprintf fmt ":bold "
  | Background c ->
    fprintf fmt ":background ";
    print_color fmt c;
    fprintf fmt " "
  | Foreground c ->
    fprintf fmt ":foreground ";
    print_color fmt c;
    fprintf fmt " "
  | Inherit s ->
    fprintf fmt ":inherit %s " s

  let print_terminal fmt = function
    All f -> 
      fprintf fmt "\t\t(t (";
      List.iter (print_face_option fmt) f;
      fprintf fmt "))\n"
  | Background_dark f ->
      fprintf fmt "\t\t(((background dark)) (";
      List.iter (print_face_option fmt) f;
      fprintf fmt "))\n"
  | Background_light f ->
      fprintf fmt "\t\t(((background light)) (";
      List.iter (print_face_option fmt) f;
      fprintf fmt "))\n"

  let print_face: formatter -> face -> unit = fun fmt face ->
    fprintf fmt "(defface %s\n" face.name;
    fprintf fmt "\t'(\n";
    List.iter (print_terminal fmt) face.terminals;
    fprintf fmt "\t)\n";
    fprintf fmt "\t\"%s\"\n" face.description;
    fprintf fmt "\t:group 'ligo\n";
    fprintf fmt ")\n";
    fprintf fmt "(defvar %s\n" face.name;
    fprintf fmt "\t'%s)\n\n" face.name
    
  let print_faces fmt =
    let faces = [
      LigoFontLock.character;
      LigoFontLock.number;
      LigoFontLock.float_;
      LigoFontLock.builtin_function;
      LigoFontLock.statement;
      LigoFontLock.conditional;
      LigoFontLock.repeat;
      LigoFontLock.label;
      LigoFontLock.operator;
      LigoFontLock.exception_;
      LigoFontLock.builtin_type;
      LigoFontLock.storage_class;
      LigoFontLock.builtin_module;
      LigoFontLock.structure;
      LigoFontLock.type_def;
      LigoFontLock.special_char;
      LigoFontLock.special_comment;
      LigoFontLock.error;
      LigoFontLock.todo
    ]
    in
    List.iter (fun f -> print_face fmt f) faces

  let print_syntax_table _fmt = 
    2
    (*
      (defun ligo-syntax-table ()
  "Common syntax table for all LIGO dialects."
  (let ((st (make-syntax-table)))
    ;; Identifiers
    (modify-syntax-entry ?_ "_" st)
    (modify-syntax-entry ?' "_" st)
    (modify-syntax-entry ?. "'" st)

    ;; Punctuation
    (dolist (c '(?# ?! ?$ ?% ?& ?+ ?- ?/ ?: ?< ?= ?> ?@ ?^ ?| ?? ?~))
      (modify-syntax-entry c "." st))

    ;; Quotes
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\\ "\\" st)

    ;; Comments are different in dialects, so they should be added
    ;; by dialect-specific syntax tables
    st))
    *)


end

module Convert = struct 
  
  (* let pattern_to_emacs: Textmate.pattern -> _ = function
    {name; kind = Match {match_; match_name; captures}} ->
       *)

end

let to_emacs t =
  ignore t;
  (* let v = Convert.to_vim t in *)
  let buffer = Buffer.create 100 in
  let open Format in
  let fmt = formatter_of_buffer buffer in
  Print.print_faces fmt;
  (* fprintf fmt "if exists(\"b:current_syntax\")\n";
  fprintf fmt "    finish\n";
  fprintf fmt "endif\n";
  Print.print fmt v;
  let name = match Filename.extension t.scope_name with 
      "" -> t.scope_name
    | a -> String.sub a 1 (String.length a - 1)
  in
  fprintf fmt "\nlet b:current_syntax = \"%s\"" name; *)
  Buffer.contents buffer


  (* Print.print_faces 
  print_endline "onwards to emacs!";
  ignore t;
  "todo x" *)