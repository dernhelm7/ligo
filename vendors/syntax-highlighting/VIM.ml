[@@@warning "-27"]
let pp = Rex.Vim.pp

type 'a command = {
  group_name: string;
  value: 'a;
  contained: bool;
  contained_in: string list;
  next_groups: string list
}

type keyword = string list command 

type match_ = string command

type match_group = {
  match_group_name: string;
  start:  string option;
  end_:   string option
}

type region_inside = {
  start: string option;
  end_: string option;
  contains: string list;
  match_groups: match_group list
}

type region = region_inside command

type syntax = 
  Match of match_
| Region of region

type link = {
  group_name: string;
  highlight: Textmate.highlight_name
}

type highlight = 
  Link of link

type item = 
  Syntax of syntax
| Highlight of highlight
| VIMComment of string

type t = item list

module Print = struct 
  open Format

  let print_list fmt prefix = function 
      [] -> ()
    | _ as l ->
      fprintf fmt prefix;
      pp_print_list ~pp_sep:(fun fmt a -> fprintf fmt ",") (fun fmt a -> fprintf fmt "%s" a) fmt l;
      fprintf fmt " "

  let rec print_syntax fmt = function
    Match {group_name; value=regexp; contained; contained_in; next_groups} ->
      fprintf fmt "syntax match %s \"%s\" " group_name regexp;
      if contained then 
        fprintf fmt "contained ";
      print_list fmt "containedin=" contained_in;
      print_list fmt "nextgroup=" next_groups;
      fprintf fmt "\n"
  | Region {group_name; value={start; end_; contains; match_groups}; next_groups; contained; contained_in; } ->
      fprintf fmt "syntax region %s " group_name;
      print_match_groups fmt match_groups;
      (match start with 
        Some start -> fprintf fmt "start=\"%s\" " start;
      | None -> ());
      (match end_ with 
        Some end_ -> fprintf fmt "end=\"%s\" " end_;
      | None -> ());
      if contained then 
        fprintf fmt "contained ";
      print_list fmt "containedin=" contained_in;
      print_list fmt "contains=" contains;
      print_list fmt "nextgroup=" next_groups;
      fprintf fmt "\n"  

    and print_match_groups fmt = function 
      {match_group_name; start; end_} :: rest -> 
        fprintf fmt "matchgroup=%s " match_group_name;
        (match start with 
          Some s -> fprintf fmt "start=\"%s\" " s
        | None -> ());
        (match end_ with 
          Some s -> fprintf fmt "end=\"%s\" " s
        | None -> ());
        print_match_groups fmt rest
    | [] -> ()

    let highlight_to_string = function
      Textmate.Comment -> "Comment"
    | Constant         -> "Constant"
    | String           -> "String"
    | Character        -> "Character"
    | Number           -> "Number"
    | Boolean          -> "Boolean"
    | Float            -> "Float"
    | FunctionName
    | Identifier       -> "Identifier"
    | Builtin_function
    | Function         -> "Function"
    | Statement        -> "Statement"
    | Conditional      -> "Conditional"
    | Repeat           -> "Repeat"
    | Label            -> "Label"
    | Operator         -> "Operator"
    | Keyword          -> "Keyword"
    | Exception        -> "Exception"
    | PreProc          -> "PreProc"
    | Builtin_type
    | Type             -> "Type"
    | StorageClass     -> "StorageClass"
    | Builtin_module
    | Structure        -> "Structure"
    | Typedef          -> "Typedef"
    | SpecialChar      -> "SpecialChar"
    | SpecialComment   -> "SpecialComment"
    | Underlined       -> "Underlined"
    | Error            -> "Error"
    | Todo             -> "Todo"


    let print_highlight fmt = function
      Link {group_name; highlight} ->
        fprintf fmt "highlight link %s %s \n" group_name (highlight_to_string highlight)

    let print_comment fmt comment = 
      fprintf fmt "\n\" %s\n" comment

    let print fmt v = 
      List.iter (fun i -> 
        match i with 
          VIMComment c -> print_comment fmt c
        | Syntax s -> print_syntax fmt s
        | Highlight h -> print_highlight fmt h
      ) v
end

module Convert = struct 
  let regexp_lookbehind     = Str.regexp "?<="
  let regexp_lookahead      = Str.regexp "?="
  let regexp_lookahead_neg  = Str.regexp "?!"
  let regexp_at             = Str.regexp "@"
  let regexp_word_boundary  = Str.regexp "\\\\b"
  let regexp_lbrace         = Str.regexp "{"
  let regexp_quote          = Str.regexp "\""
  let regexp_equals         = Str.regexp "="

  let textmate_regexp_to_vim_regexp = fun r -> 
    let r = Str.global_replace regexp_word_boundary "" r in
    let r = Str.global_replace regexp_lookbehind "@<=" r in
    let r = Str.global_replace regexp_lookahead "@=" r in
    let r = Str.global_replace regexp_lookahead_neg "@!" r in
    let r = Str.global_replace regexp_at "\\@" r in
    let r = Str.global_replace regexp_lbrace "\\{" r in
    let r = Str.global_replace regexp_quote "\\\"" r in
    "\\v" ^ r

  let pattern_to_vim: string list -> Textmate.pattern -> item list = fun toplevel -> function
    {name; kind = Begin_end {meta_name=highlight; begin_; end_; patterns}} ->
    let rec aux name result begin_ end_ = (
      match begin_, end_ with 
        (start, highlight_start) :: [], (end_, highlight_end) :: rest ->
          aux 
            (match rest with [] -> name | _ -> name ^ "___") 
            (
              (match highlight_end with Some highlight -> 
                [Highlight (Link {group_name = name ^ "__"; highlight})]
              | None -> []) @
              (match highlight_start with Some highlight -> 
                [Highlight (Link {group_name = name ^ "_"; highlight})]
              | None -> []) @
              (match highlight with Some highlight -> 
                [Highlight (Link {group_name = name; highlight})]
              | None -> []) @
              Syntax (Region {
              group_name = name;
              value = ({                
                start = (match highlight_start with Some _ -> None | None -> Some (textmate_regexp_to_vim_regexp start));
                end_ = (match highlight_end with Some _ -> None | None -> Some (textmate_regexp_to_vim_regexp end_));
                contains = patterns;
                match_groups = 
                  (match highlight_start with 
                    Some _ -> [{
                      match_group_name = name ^ "_";
                      start =  Some (textmate_regexp_to_vim_regexp start);
                      end_  =   None
                    }]
                  | None -> [])
                  @
                  (match highlight_end with 
                    Some _ -> [{
                      match_group_name = name ^ "__";
                      start =  None; 
                      end_  =  Some (textmate_regexp_to_vim_regexp end_);
                    }]
                  | None -> [])
              }: region_inside);
              next_groups = (match rest with [] -> [] | _ -> [name ^ "___"]);
              contained = not (List.mem name toplevel);
              contained_in = [];
            }) :: 
          result) [] rest
      | (match_, highlight) :: rest, end_ -> 
          aux 
            (name ^ "___" )
            (
              (match highlight with Some highlight -> [Highlight (Link {group_name = name; highlight})] | None -> [])
              @ 
              Syntax (Match {
                group_name = name;
                value = textmate_regexp_to_vim_regexp match_;
                next_groups = [name ^ "___"];
                contained = not (List.mem name toplevel);
                contained_in = []; 
              })
              :: 
             result
            ) rest end_
      | [], (match_, highlight) :: rest_e -> 
        aux 
        (name ^ "___" )
        (
          (match highlight with Some highlight -> [Highlight (Link {group_name = name; highlight})] | None -> [])
          @ 
          Syntax (Match {
            group_name = name;
            value = textmate_regexp_to_vim_regexp match_;
            next_groups = (match rest_e with [] -> [] | _ -> [name ^ "___"]);
            contained = not (List.mem name toplevel);
            contained_in = []; 
          })
          :: 
          result
        )
        [] 
        rest_e
      | [], [] -> 
        List.rev result
    )
      in
    [
      VIMComment name;
    ]
    @ 
    aux name [] begin_ end_
  | {name; kind = Match {match_; match_name; captures}} -> 
    [VIMComment name;
    Syntax (Match {
      group_name = name;
      value = pp match_;
      contained = not (List.mem name toplevel);
      contained_in = [];
      next_groups = []
    })]
    @
    (match match_name with 
      Some highlight -> [Highlight (Link {group_name = name; highlight})]
    | None -> [])

  let to_vim: Textmate.t -> t = fun t ->
    let toplevel = t.syntax_patterns in
    List.fold_left (fun a c -> (pattern_to_vim toplevel c) @ a ) [] t.repository
    
end
  
let to_vim: Textmate.t -> string = fun t ->
  let v = Convert.to_vim t in
  let buffer = Buffer.create 100 in
  let open Format in
  let fmt = formatter_of_buffer buffer in
  fprintf fmt "if exists(\"b:current_syntax\")\n";
  fprintf fmt "    finish\n";
  fprintf fmt "endif\n";
  Print.print fmt v;
  let name = match Filename.extension t.scope_name with 
      "" -> t.scope_name
    | a -> String.sub a 1 (String.length a - 1)
  in
  fprintf fmt "\nlet b:current_syntax = \"%s\"" name;
  Buffer.contents buffer

