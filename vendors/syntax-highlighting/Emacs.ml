
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


let highlight_to_opt = function
      Textmate.Comment -> Some "font-lock-comment-face"
    | Constant         -> Some "font-lock-constant-face"
    | String           -> Some "font-lock-string-face"
    | Character        
    | Number           
    | Boolean          
    | Float            
    | Identifier       -> None
    | Builtin_function -> Some "font-lock-builtin-face"
    | Function         -> Some "font-lock-function-name-face"
    | Statement        -> Some "font-lock-keyword-face"
    | Conditional      -> Some "font-lock-keyword-face"
    | Repeat           -> Some "font-lock-keyword-face"
    | Label            -> None
    | Operator         -> None
    | Keyword          -> Some "font-lock-keyword-face"
    | Exception        -> Some "font-lock-keyword-face"
    | PreProc          -> Some "font-lock-preprocessor-face"
    | Builtin_type     -> Some "font-lock-builtin-face"
    | Type             -> Some "font-lock-type-face"
    | StorageClass     -> Some "font-lock-keyword-face"
    | Builtin_module   -> Some "font-lock-builtin-face"
    | Structure        -> Some "font-lock-keyword-face"
    | Typedef          -> Some "font-lock-type-face"
    | SpecialChar      -> None
    | SpecialComment   -> Some "font-lock-doc-face"
    | Underlined       -> None
    | Error            -> Some "font-lock-doc-face"
    | Todo             -> Some "font-lock-doc-face"

module Print = struct

end