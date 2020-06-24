open Types
open Combinators

module Free_variables = struct

  type bindings = expression_variable list
  let mem : expression_variable -> bindings -> bool = List.mem
  let singleton : expression_variable -> bindings = fun s -> [ s ]
  let mem_count : expression_variable -> bindings -> int =
    fun x fvs ->
    List.length (List.filter (Var.equal x) fvs)
  let union : bindings -> bindings -> bindings = (@)
  let unions : bindings list -> bindings = List.concat
  let empty : bindings = []
  let of_list : expression_variable list -> bindings = fun x -> x

  let rec expression : bindings -> expression -> bindings = fun b e ->
    let self = expression b in
    match e.content with
    | E_literal v -> value b v
    | E_closure f -> lambda b f
    | E_skip -> empty
    | E_constant (c) -> unions @@ List.map self c.arguments
    | E_application (f, x) -> unions @@ [ self f ; self x ]
    | E_variable n -> var_name b n
    | E_make_none _ -> empty
    | E_iterator (_, ((v, _), body), expr) ->
      unions [ expression (union (singleton v) b) body ;
               self expr ;
             ]
    | E_fold (((v, _), body), collection, initial) ->
      unions [ expression (union (singleton v) b) body ;
               self collection ;
               self initial ;
             ]
    | E_if_bool (x, bt, bf) -> unions [ self x ; self bt ; self bf ]
    | E_if_none (x, bn, ((s, _), bs)) ->
      unions [ self x ;
               self bn ;
               expression (union (singleton s) b) bs ;
             ]
    | E_if_cons (x, bnil , (((h, _) , (t, _)) , bcons)) ->
      unions [ self x ;
               self bnil ;
               expression (unions [ singleton h ; singleton t ; b ]) bcons ;
             ]
    | E_if_left (x, ((l, _), bl), ((r, _), br)) ->
      unions [ self x ;
               expression (union (singleton l) b) bl ;
               expression (union (singleton r) b) br ;
             ]
    | E_let_in ((v , _) , _, expr , body) ->
      unions [ self expr ;
               expression (union (singleton v) b) body ;
             ]
    | E_sequence (x, y) -> union (self x) (self y)
    | E_record_update (r, _,e) -> union (self r) (self e)
    | E_while (cond , body) -> union (self cond) (self body)
    | E_raw_michelson _ -> empty

  and var_name : bindings -> var_name -> bindings = fun b n ->
    if mem n b
    then empty
    else singleton n

  and value : bindings -> value -> bindings = fun b v ->
    let self = value b in
    match v with
    | D_unit
    | D_bool _
    | D_nat _
    | D_timestamp _
    | D_mutez _
    | D_int _
    | D_string _
    | D_bytes _
    | D_none
    | D_operation _
      -> empty
    | D_pair (x, y) -> unions [ self x ; self y ]
    | D_left x
    | D_right x
    | D_some x
      -> self x
    | D_map kvs
    | D_big_map kvs
      -> unions @@ List.map (fun (k, v) -> unions [ self k ; self v ]) kvs
    | D_list xs
    | D_set xs
      -> unions @@ List.map self xs

  and lambda : bindings -> anon_function -> bindings = fun b l ->
    let b = union (singleton l.binder) b in
    expression b l.body

end

let get_entry (lst : program) (name : string) : (expression * int) option =
  let entry_expression =
    let aux x =
      let (((decl_name , _, decl_expr) , _)) = x in
      if (Var.equal decl_name (Var.of_name name))
      then Some decl_expr
      else None
    in
    List.find_map aux (List.rev lst)
  in
  match entry_expression with
    | Some exp ->
      let entry_index =
        let aux x =
          let (((decl_name , _, _) , _)) = x in
          Var.equal decl_name (Var.of_name name)
        in
        (List.length lst) - (List.find_index aux (List.rev lst)) - 1
      in
      Some (exp, entry_index)
    | None -> None

type form_t =
  | ContractForm of expression
  | ExpressionForm of expression

let aggregate_entry (lst : program) (form : form_t) : expression option =
  let wrapper =
    let aux prec cur =
      let (((name , inline, expr) , _)) = cur in
      e_let_in name expr.type_expression inline expr prec
    in
    fun expr -> List.fold_right' aux expr lst
  in
  match form with
  | ContractForm entry_expression -> (
    match (entry_expression.content) with
    | (E_closure l) -> (
        let l' = { l with body = wrapper l.body } in
        let e' = {
          content = E_closure l' ;
          type_expression = entry_expression.type_expression ;
          location = entry_expression.location;
        } in
        Some e'
      )
    | _ -> None )
  | ExpressionForm entry_expression ->
    Some (wrapper entry_expression)