open Ast_typed
open Trace
open Ast_typed.Helpers

type 'a folder = 'a -> expression -> 'a result
let rec fold_expression : 'a . 'a folder -> 'a -> expression -> 'a result = fun f init e ->
  let self = fold_expression f in 
  let%bind init' = f init e in
  match e.expression_content with
  | E_literal _ | E_variable _ -> ok init'
  | E_constant {arguments=lst} -> (
    let%bind res = bind_fold_list self init' lst in
    ok res
  )
  | E_application {lamb; args} -> (
      let ab = (lamb, args) in
      let%bind res = bind_fold_pair self init' ab in
      ok res
    )
  | E_lambda { binder = _ ; result = e }
  | E_recursive {lambda= {result=e}}
  | E_constructor {element=e} -> (
      let%bind res = self init' e in
      ok res
    )
  | E_matching {matchee=e; cases} -> (
      let%bind res = self init' e in
      let%bind res = fold_cases f res cases in
      ok res
    )
  | E_record m -> (
    let aux init'' _ expr =
      let%bind res = fold_expression self init'' expr in
      ok res
    in
    let%bind res = bind_fold_lmap aux (ok init') m in
    ok res
  )
  | E_record_update {record;update} -> (
    let%bind res = self init' record in
    let%bind res = fold_expression self res update in
    ok res 
  )
  | E_record_accessor {record} -> (
     let%bind res = self init' record in
     ok res
    )
  | E_let_in { let_binder = _ ; rhs ; let_result } -> (
      let%bind res = self init' rhs in
      let%bind res = self res let_result in
      ok res
    )

and fold_cases : 'a . 'a folder -> 'a -> matching_expr -> 'a result = fun f init m ->
  match m with
  | Match_list { match_nil ; match_cons = {hd=_; tl=_ ; body; tv=_} } -> (
      let%bind res = fold_expression f init match_nil in
      let%bind res = fold_expression f res body in
      ok res
    )
  | Match_option { match_none ; match_some = {opt=_; body; tv=_} } -> (
      let%bind res = fold_expression f init match_none in
      let%bind res = fold_expression f res body in
      ok res
    )
  | Match_tuple {vars=_ ; body; tvs=_} -> (
      let%bind res = fold_expression f init body in
      ok res
    )
  | Match_variant {cases;tv=_} -> (
      let aux init' {constructor=_; pattern=_ ; body} =
        let%bind res' = fold_expression f init' body in
        ok res' in
      let%bind res = bind_fold_list aux init cases in
      ok res
    )

type mapper = expression -> expression result
let rec map_expression : mapper -> expression -> expression result = fun f e ->
  let self = map_expression f in
  let%bind e' = f e in
  let return expression_content = ok { e' with expression_content } in
  match e'.expression_content with
  | E_matching {matchee=e;cases} -> (
      let%bind e' = self e in
      let%bind cases' = map_cases f cases in
      return @@ E_matching {matchee=e';cases=cases'}
    )
  | E_record_accessor {record; path} -> (
      let%bind record = self record in
      return @@ E_record_accessor {record; path}
    )
  | E_record m -> (
    let%bind m' = bind_map_lmap self m in
    return @@ E_record m'
  )
  | E_record_update {record; path; update} -> (
    let%bind record = self record in
    let%bind update = self update in
    return @@ E_record_update {record;path;update}
  )
  | E_constructor c -> (
      let%bind e' = self c.element in
      return @@ E_constructor {c with element = e'}
  )
  | E_application {lamb; args} -> (
      let ab = (lamb, args) in
      let%bind (a,b) = bind_map_pair self ab in
      return @@ E_application {lamb=a;args=b}
    )
  | E_let_in { let_binder ; rhs ; let_result; inline } -> (
      let%bind rhs = self rhs in
      let%bind let_result = self let_result in
      return @@ E_let_in { let_binder ; rhs ; let_result; inline }
    )
  | E_lambda { binder ; result } -> (
      let%bind result = self result in
      return @@ E_lambda { binder ; result }
    )
  | E_recursive { fun_name; fun_type; lambda = {binder;result}} -> (
      let%bind result = self result in
      return @@ E_recursive { fun_name; fun_type; lambda = {binder;result}}
    )
  | E_constant c -> (
      let%bind args = bind_map_list self c.arguments in
      return @@ E_constant {c with arguments=args}
    )
  | E_literal _ | E_variable _ as e' -> return e'


and map_cases : mapper -> matching_expr -> matching_expr result = fun f m ->
  match m with
  | Match_list { match_nil ; match_cons = {hd ; tl ; body ; tv} } -> (
      let%bind match_nil = map_expression f match_nil in
      let%bind body = map_expression f body in
      ok @@ Match_list { match_nil ; match_cons = {hd ; tl ; body; tv} }
    )
  | Match_option { match_none ; match_some = {opt ; body ; tv } } -> (
      let%bind match_none = map_expression f match_none in
      let%bind body = map_expression f body in
      ok @@ Match_option { match_none ; match_some = { opt ; body ; tv } }
    )
  | Match_tuple { vars ; body ; tvs } -> (
      let%bind body = map_expression f body in
      ok @@ Match_tuple { vars ; body ; tvs }
    )
  | Match_variant {cases;tv} -> (
      let aux { constructor ; pattern ; body } =
        let%bind body = map_expression f body in
        ok {constructor;pattern;body}
      in
      let%bind cases = bind_map_list aux cases in
      ok @@ Match_variant {cases ; tv}
    )

and map_program : mapper -> program -> program result = fun m p ->
  let aux = fun (x : declaration) ->
    match x with
    | Declaration_constant {binder; expr ; inline ; post_env} -> (
        let%bind expr = map_expression m expr in
        ok (Declaration_constant {binder; expr ; inline ; post_env})
      )
  in
  bind_map_list (bind_map_location aux) p

type 'a fold_mapper = 'a -> expression -> (bool * 'a * expression) result
let rec fold_map_expression : 'a . 'a fold_mapper -> 'a -> expression -> ('a * expression) result = fun f a e ->
  let self = fold_map_expression f in
  let%bind (continue, init',e') = f a e in
  if (not continue) then ok(init',e')
  else
  let return expression_content = { e' with expression_content } in
  match e'.expression_content with
  | E_matching {matchee=e;cases} -> (
      let%bind (res, e') = self init' e in
      let%bind (res,cases') = fold_map_cases f res cases in
      ok (res, return @@ E_matching {matchee=e';cases=cases'})
    )
  | E_record_accessor {record; path} -> (
      let%bind (res, record) = self init' record in
      ok (res, return @@ E_record_accessor {record; path})
    )
  | E_record m -> (
    let%bind (res, lst') = bind_fold_map_list (fun res (k,e) -> let%bind (res,e) = self res e in ok (res,(k,e))) init' (LMap.to_kv_list m) in
    let m' = LMap.of_list lst' in
    ok (res, return @@ E_record m')
  )
  | E_record_update {record; path; update} -> (
    let%bind (res, record) = self init' record in
    let%bind (res, update) = self res update in
    ok (res, return @@ E_record_update {record;path;update})
  )
  | E_constructor c -> (
      let%bind (res,e') = self init' c.element in
      ok (res, return @@ E_constructor {c with element = e'})
  )
  | E_application {lamb;args} -> (
      let ab = (lamb, args) in
      let%bind (res,(a,b)) = bind_fold_map_pair self init' ab in
      ok (res, return @@ E_application {lamb=a;args=b})
    )
  | E_let_in { let_binder ; rhs ; let_result; inline } -> (
      let%bind (res,rhs) = self init' rhs in
      let%bind (res,let_result) = self res let_result in
      ok (res, return @@ E_let_in { let_binder ; rhs ; let_result ; inline })
    )
  | E_lambda { binder ; result } -> (
      let%bind (res,result) = self init' result in
      ok ( res, return @@ E_lambda { binder ; result })
    )
  | E_recursive { fun_name; fun_type; lambda={binder;result}} -> (
      let%bind (res,result) = self init' result in
      ok (res, return @@ E_recursive {fun_name; fun_type; lambda={binder;result}})
    )
  | E_constant c -> (
      let%bind (res,args) = bind_fold_map_list self init' c.arguments in
      ok (res, return @@ E_constant {c with arguments=args})
    )
  | E_literal _ | E_variable _ as e' -> ok (init', return e')

and fold_map_cases : 'a . 'a fold_mapper -> 'a -> matching_expr -> ('a * matching_expr) result = fun f init m ->
  match m with
  | Match_list { match_nil ; match_cons = { hd ; tl ; body ; tv } } -> (
      let%bind (init, match_nil) = fold_map_expression f init match_nil in
      let%bind (init, body) = fold_map_expression f init body in
      ok @@ (init, Match_list { match_nil ; match_cons = { hd ; tl ; body ; tv } })
    )
  | Match_option { match_none ; match_some = { opt ; body ; tv } } -> (
      let%bind (init, match_none) = fold_map_expression f init match_none in
      let%bind (init, body) = fold_map_expression f init body in
      ok @@ (init, Match_option { match_none ; match_some = { opt ; body ; tv } })
    )
  | Match_tuple { vars ; body ; tvs } -> (
      let%bind (init, body) = fold_map_expression f init body in
      ok @@ (init, Match_tuple {vars ; body ; tvs })
    )
  | Match_variant {cases ; tv} -> (
      let aux init {constructor ; pattern ; body} =
        let%bind (init, body) = fold_map_expression f init body in
        ok (init, {constructor; pattern ; body})
      in
      let%bind (init,cases) = bind_fold_map_list aux init cases in
      ok @@ (init, Match_variant {cases ; tv})
    )

and fold_map_program : 'a . 'a fold_mapper -> 'a -> program -> ('a * program) result = fun m init p ->
  let aux = fun (acc,acc_prg) (x : declaration Location.wrap) ->
    match Location.unwrap x with
    | Declaration_constant {binder ; expr ; inline ; post_env} -> (
        let%bind (acc', expr) = fold_map_expression m acc expr in
        let wrap_content = Declaration_constant {binder ; expr ; inline ; post_env} in
        ok (acc', List.append acc_prg [{x with wrap_content}])
      )
  in
  bind_fold_list aux (init,[]) p

module Errors = struct
  let bad_contract_io entrypoint (e:expression) () =
    let title = thunk "badly typed contract" in
    let message () = Format.asprintf "unexpected entrypoint type" in
    let data = [
      ("location" , fun () -> Format.asprintf "%a" Location.pp e.location);
      ("entrypoint" , fun () -> entrypoint);
      ("entrypoint_type" , fun () -> Format.asprintf "%a" Ast_typed.PP.type_expression e.type_expression)
    ] in
    error ~data title message ()

  let expected_list_operation entrypoint got (e:expression) () =
    let title = thunk "bad return type" in
    let message () = Format.asprintf "expected %a, got %a"
      Ast_typed.PP.type_expression {got with type_content= T_operator (TC_list {got with type_content=T_constant TC_operation})}
      Ast_typed.PP.type_expression got
    in
    let data = [
      ("location" , fun () -> Format.asprintf "%a" Location.pp e.location);
      ("entrypoint" , fun () -> entrypoint)
    ] in
    error ~data title message ()

  let expected_same entrypoint t1 t2 (e:expression) () =
    let title = thunk "badly typed contract" in
    let message () = Format.asprintf "expected {%a} and {%a} to be the same in the entrypoint type"
      Ast_typed.PP.type_expression t1
      Ast_typed.PP.type_expression t2
    in
    let data = [
      ("location" , fun () -> Format.asprintf "%a" Location.pp e.location);
      ("entrypoint" , fun () -> entrypoint);
      ("entrypoint_type" , fun () -> Format.asprintf "%a" Ast_typed.PP.type_expression e.type_expression)
    ] in
    error ~data title message ()
  
end

type contract_type = {
  parameter : Ast_typed.type_expression ;
  storage : Ast_typed.type_expression ;
}

let fetch_contract_type : string -> program -> contract_type result = fun main_fname program ->
  let main_decl = List.rev @@ List.filter
    (fun declt ->
      let (Declaration_constant { binder ; expr=_ ; inline=_ ; post_env=_ }) = Location.unwrap declt in
      String.equal (Var.to_name binder) main_fname
    )
    program
  in
  match main_decl with
  | (hd::_) -> (
    let (Declaration_constant { binder=_ ; expr ; inline=_ ; post_env=_ }) = Location.unwrap hd in
    match expr.type_expression.type_content with
    | T_arrow {type1 ; type2} -> (
      match type1.type_content , type2.type_content with
      | T_record tin , T_record tout when (is_tuple_lmap tin) && (is_tuple_lmap tout) ->
        let%bind (parameter,storage) = Ast_typed.Helpers.get_pair tin in
        let%bind (listop,storage') = Ast_typed.Helpers.get_pair tout in
        let%bind () = trace_strong (Errors.expected_list_operation main_fname listop expr) @@
          Ast_typed.assert_t_list_operation listop in
        let%bind () = trace_strong (Errors.expected_same main_fname storage storage' expr) @@
          Ast_typed.assert_type_expression_eq (storage,storage') in
        (* TODO: on storage/parameter : assert_storable, assert_passable ? *)
        ok { parameter ; storage }
      |  _ -> fail @@ Errors.bad_contract_io main_fname expr
      )
    | _ -> fail @@ Errors.bad_contract_io main_fname expr
  )
  | [] -> simple_fail ("Entrypoint '"^main_fname^"' does not exist")