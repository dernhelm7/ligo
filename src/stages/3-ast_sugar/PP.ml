[@@@coverage exclude_file]
open Types
open Format
open PP_helpers

include Stage_common.PP

(* TODO: move to common *)
let lmap_sep value sep ppf m =
  let lst = LMap.to_kv_list m in
  let lst = List.sort (fun (Label a,_) (Label b,_) -> String.compare a b) lst in
  let new_pp ppf (k, {associated_type;_}) = fprintf ppf "@[<h>%a -> %a@]" label k value associated_type in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let lmap_sep_d x = lmap_sep x (tag " ,@ ")

let record_sep_t value sep ppf (m : 'a label_map) =
  let lst = LMap.to_kv_list m in
  let lst = List.sort_uniq (fun (Label a,_) (Label b,_) -> String.compare a b) lst in
  let new_pp ppf (k, {associated_type;_}) = fprintf ppf "@[<h>%a -> %a@]" label k value associated_type in
  fprintf ppf "%a" (list_sep new_pp sep) lst


let expression_variable ppf (ev : expression_variable) : unit =
  fprintf ppf "%a" Var.pp ev.wrap_content

let list_sep_d_par f ppf lst =
  match lst with
  | [] -> ()
  | _ -> fprintf ppf " (%a)" (list_sep_d f) lst

let rec type_content : formatter -> type_expression -> unit =
  fun ppf te ->
  match te.type_content with
  | T_variable tv -> type_variable ppf tv
  | T_sum      sm -> sum           type_expression ppf sm.fields
  | T_record   rd -> type_record   type_expression ppf rd.fields
  | T_tuple     t -> type_tuple    type_expression ppf t
  | T_arrow     a -> arrow         type_expression ppf a
  | T_app     app -> type_app      type_expression ppf app

and type_expression ppf (te : type_expression) : unit =
  fprintf ppf "%a" type_content te

let rec expression ppf (e : expression) =
  expression_content ppf e.expression_content
and expression_content ppf (ec : expression_content) =
  match ec with
  | E_literal l ->
      literal ppf l
  | E_variable n ->
      fprintf ppf "%a" expression_variable n
  | E_application {lamb;args} ->
      fprintf ppf "(%a)@(%a)" expression lamb expression args
  | E_constructor c ->
      fprintf ppf "%a(%a)" label c.constructor expression c.element
  | E_constant c -> constant expression ppf c
  | E_record m ->
      fprintf ppf "{%a}" (record_sep_expr expression (const ";")) m
  | E_accessor {record;path} ->
      fprintf ppf "%a.%a" expression record (list_sep accessor (const ".")) path
  | E_update {record; path; update} ->
      fprintf ppf "{ %a with %a = %a }" expression record (list_sep accessor (const ".")) path expression update
  | E_map m ->
      fprintf ppf "map[%a]" (list_sep_d (assoc_expression expression)) m
  | E_big_map m ->
      fprintf ppf "big_map[%a]" (list_sep_d (assoc_expression expression)) m
  | E_list lst ->
      fprintf ppf "list[%a]" (list_sep_d expression) lst
  | E_set lst ->
      fprintf ppf "set[%a]" (list_sep_d expression) lst
  | E_lambda {binder; output_type; result} ->
      fprintf ppf "lambda (%a) : %a return %a"
        option_type_name binder
        (PP_helpers.option type_expression) output_type
        expression result
  | E_recursive { fun_name; fun_type; lambda} ->
      fprintf ppf "rec (%a:%a => %a )"
        expression_variable fun_name
        type_expression fun_type
        expression_content (E_lambda lambda)
  | E_matching {matchee; cases; _} ->
      fprintf ppf "match %a with %a" expression matchee (matching expression)
        cases
  | E_let_in { let_binder ; rhs ; let_result; attributes=attr; mut} ->
      fprintf ppf "let %a%a = %a%a in %a"
        option_type_name let_binder
        option_mut mut
        expression rhs
        attributes attr
        expression let_result
  | E_type_in   ti -> type_in expression type_expression ppf ti
  | E_raw_code {language; code} ->
      fprintf ppf "[%%%s %a]" language expression code
  | E_ascription {anno_expr; type_annotation} ->
      fprintf ppf "%a : %a" expression anno_expr type_expression type_annotation
  | E_cond {condition; then_clause; else_clause} ->
      fprintf ppf "if %a then %a else %a"
        expression condition
        expression then_clause
        expression else_clause
  | E_sequence {expr1;expr2} ->
      fprintf ppf "{ %a; @. %a}" expression expr1 expression expr2
  | E_skip ->
      fprintf ppf "skip"
  | E_tuple t ->
      fprintf ppf "(%a)" (list_sep_d expression) t


and accessor ppf a =
  match a with
    | Access_tuple i  -> fprintf ppf "%a" Z.pp_print i
    | Access_record s -> fprintf ppf "%s" s
    | Access_map e    -> fprintf ppf "%a" expression e

and option_type_name ppf {var;ascr}=
  match ascr with
  | None ->
      fprintf ppf "%a" expression_variable var
  | Some ty ->
      fprintf ppf "%a : %a" expression_variable var type_expression ty

and matching_variant_case : type a . (_ -> a -> unit) -> _ -> (label * expression_variable) * a -> unit =
  fun f ppf ((c,n),a) ->
  fprintf ppf "| %a %a -> %a" label c expression_variable n f a

and matching : (formatter -> expression -> unit) -> formatter -> matching_expr -> unit =
  fun f ppf m -> match m with
    | Match_variant lst ->
        fprintf ppf "%a" (list_sep (matching_variant_case f) (tag "@.")) lst
    | Match_list {match_nil ; match_cons = (hd, tl, match_cons)} ->
        fprintf ppf "| Nil -> %a @.| %a :: %a -> %a" f match_nil expression_variable hd expression_variable tl f match_cons
    | Match_option {match_none ; match_some = (some, match_some)} ->
        fprintf ppf "| None -> %a @.| Some %a -> %a" f match_none expression_variable some f match_some
    | Match_tuple (lst,b) ->
        fprintf ppf "(%a) -> %a" (list_sep_d option_type_name) lst f b
    | Match_record (lst,b) ->
        fprintf ppf "{%a} -> %a" (list_sep_d (fun ppf (a,b) -> fprintf ppf "%a = %a" label a option_type_name b)) lst f b
    | Match_variable (a,b) ->
        fprintf ppf "%a -> %a" option_type_name a f b

(* Shows the type expected for the matched value *)
and matching_type ppf m = match m with
  | Match_variant lst ->
      fprintf ppf "variant %a" (list_sep matching_variant_case_type (tag "@.")) lst
  | Match_list _ ->
      fprintf ppf "list"
  | Match_option _ ->
      fprintf ppf "option"
  | Match_tuple _ ->
      fprintf ppf "tuple"
  | Match_record _ ->
      fprintf ppf "record"
  | Match_variable _ ->
      fprintf ppf "variable"

and matching_variant_case_type ppf ((c,n),_a) =
  fprintf ppf "| %a %a" label c expression_variable n

and option_mut ppf mut =
  if mut then
    fprintf ppf "[@mut]"
  else
    fprintf ppf ""

and attributes ppf attributes =
  let attr =
    List.map (fun attr -> "[@@" ^ attr ^ "]") attributes |> String.concat ""
  in fprintf ppf "%s" attr

let declaration ppf (d : declaration) =
  match d with
  | Declaration_type     dt -> declaration_type                type_expression ppf dt
  | Declaration_constant dc -> declaration_constant expression type_expression ppf dc

let program ppf (p : program) = program declaration ppf p
