(**

This implements the pattern_matching compiler of `Peyton-Jones, S.L., The Implementation of Functional Programming Languages`, chapter 5.
By reduction, this algorithm transforms pattern matching expression into (nested) cases expressions.
`Sugar` match expression being 'pattern matching' expression and `Core`/`Typed` being 'case expressions'.

List patterns are treated as the variant type `NIL | Cons of (hd , tl)` would be.
"Product patterns" (e.g. tuple & record) are considered variables, but an extra rule (product_rule) was necessary to handle them

**)

module I = Ast_core
module O = Ast_typed

open Trace
open Typer_common.Errors

type matchees = O.expression_variable list
type pattern = I.type_expression I.pattern 
type typed_pattern = pattern * O.type_expression
type equations = (typed_pattern list * (I.expression * O.environment)) list
type type_fun =
  O.environment -> ?tv_opt:O.type_expression -> I.expression -> (O.expression, typer_error) result
type rest = O.expression_content
type 'a pm_result = ('a, typer_error) result

let is_var : _ I.pattern -> bool = fun p ->
  match p.wrap_content with
  | P_var _ -> true
  | P_tuple _ -> true
  | P_record _ -> true
  | P_unit -> true
  | _ -> false
let is_product' : _ I.pattern -> bool = fun p ->
  match p.wrap_content with
  | P_tuple _ -> true
  | P_record _ -> true
  | _ -> false

let is_product : equations -> typed_pattern option = fun eqs ->
  List.find_map
    (fun (pl,_) ->
      match pl with
      | (p,t)::_ -> if is_product' p then Some(p,t) else None 
      | [] -> None
    )
    eqs

let corner_case loc = fail (corner_case ("broken invariant at "^loc))

let list_sep_x x = let open Simple_utils.PP_helpers in list_sep x (tag "@,")
let pp_matchees : Format.formatter -> O.expression_variable list -> unit =
  fun ppf lst ->
    let lst = List.map (fun (e:O.expression_variable) -> e.wrap_content) lst in
    Format.fprintf ppf "@[%a@]" (Simple_utils.PP_helpers.list_sep_d_par Var.pp) lst

let pp_patterns : Format.formatter -> typed_pattern list -> unit =
  fun ppf lst ->
    let patterns = List.map fst lst in
    Format.fprintf ppf "@[ [ %a ]@]" (Simple_utils.PP_helpers.list_sep_d (Stage_common.PP.match_pattern I.PP.type_expression)) patterns

let pp_eq : Format.formatter ->  (typed_pattern list * (I.expression * O.environment)) -> unit =
  fun ppf (pl,(body,_env)) ->
    Format.fprintf ppf "%a -> %a" pp_patterns pl I.PP.expression body

let pp_eqs : Format.formatter -> equations -> unit =
  fun ppf lst ->
    Format.fprintf ppf "@[<v>[@,%a@,] @]" (list_sep_x pp_eq) lst

let assert_body_t : body_t:O.type_expression option -> Location.t -> O.type_expression -> unit pm_result =
  fun ~body_t loc t ->
    match body_t with
    | Some prev_t ->
      let%bind () = Typer_common.Helpers.assert_type_expression_eq loc (prev_t,t) in
      ok ()
    | None -> ok ()

let extract_variant_type : pattern -> O.label -> O.type_expression -> O.type_expression pm_result =
  fun p label t ->
  match t.type_content with
  | T_sum rows -> (
    match O.LMap.find_opt label rows.content with
    | Some t -> ok t.associated_type
    | _ -> fail @@ expected_variant p.location t
  )
  | _ -> fail @@ pattern_do_not_conform_type p t

let extract_record_type : pattern -> O.label -> O.type_expression -> O.type_expression pm_result =
  fun p label t ->
  match t.type_content with
  | T_record rows -> (
    match O.LMap.find_opt label rows.content with
    | Some t -> ok t.associated_type
    | _ -> fail @@ expected_record p.location t
  )
  | _ -> fail @@ pattern_do_not_conform_type p t

(**
get_matchee_type [ ( [ (p01,t) , .. , (p0n,t0n) ], body0 ) , .. , ( [ (pk1,t) , .. , (pkn,tkn) ], bodyk ) ]
makes sure that the left-most type/patterns pairs of the equations have the same type and return this type.
It also fails if the pattern do not conform to the type (T_sum with P_variant, T_record with P_tuple/P_record ..)
e.g.
  get_matchee_type [ ( [ (p0,t0) , ... ], body0 ) , .. , ( [ (pk,tk) , ... ], bodyk ) ]
  checks:
    - t0 = t1 = .. = tk 
    - conform p0 t0 && conform p1 t1 && conform pk tk
**)
let type_matchee : equations -> O.type_expression pm_result =
  fun eqs ->
    let pt1s = List.map (fun el -> List.hd @@ fst el) eqs in
    let conforms : typed_pattern -> unit pm_result = fun (p,t) ->
      match p.wrap_content , t.type_content with
      | I.P_var _ , _ -> ok ()
      | I.P_variant _ , O.T_sum _ -> ok ()
      | (P_tuple _ | P_record _) , O.T_record _ -> ok ()
      | I.P_unit , O.T_constant { injection ; _ } when String.equal (Ligo_string.extract injection) Stage_common.Constant.unit_name -> ok ()
      | I.P_list _ , O.T_constant { injection ; _ } when String.equal (Ligo_string.extract injection) Stage_common.Constant.list_name -> ok ()
      | _ -> fail @@ pattern_do_not_conform_type p t
    in
    let aux : O.type_expression option -> typed_pattern -> O.type_expression option pm_result = fun t_opt (p,t) ->
      let%bind () = conforms (p,t) in
      match t_opt with
      | None -> ok (Some t)
      | Some t' ->
        let%bind () = Typer_common.Helpers.assert_type_expression_eq Location.generated (t, t') in
        ok t_opt
    in
    let%bind t = bind_fold_list aux None pt1s in
    ok @@ Option.unopt_exn t

(**
  `substitute_var_in_body to_subst new_var body` replaces variables equal to `to_subst` with variable `new_var` in expression `body`.
  note that `new_var` here is never a user variable (always previously generated by the compiler)
**)
let rec substitute_var_in_body : I.expression_variable -> O.expression_variable -> I.expression -> I.expression pm_result =
  fun to_subst new_var body ->
    (* let () = Format.printf "substituting %a by %a in %a\n" I.PP.expression_variable to_subst I.PP.expression_variable new_var I.PP.expression body in *)
    let aux : unit -> I.expression -> (bool * unit * I.expression,_) result =
      fun () exp ->
        let ret continue exp = ok (continue,(),exp) in
        match exp.content with
        | I.E_variable var when Var.equal var.wrap_content to_subst.wrap_content -> ret true { exp with content = E_variable new_var }
        | I.E_let_in letin when Var.equal letin.let_binder.var.wrap_content to_subst.wrap_content ->
          let%bind rhs = substitute_var_in_body to_subst new_var letin.rhs in
          let letin = { letin with rhs } in
          ret false { exp with content = E_let_in letin}
        | I.E_lambda lamb when Var.equal lamb.binder.var.wrap_content to_subst.wrap_content -> ret false exp
        | I.E_matching m -> (
          let%bind matchee = substitute_var_in_body to_subst new_var m.matchee in
          let aux : bool -> pattern -> bool =
            fun b p ->
              match p.wrap_content with
              | P_var x when Var.equal x.var.wrap_content to_subst.wrap_content -> true
              | _ -> b
          in
          let%bind cases = bind_map_list
            (fun (case : _ I.match_case) ->
              match Stage_common.Helpers.fold_pattern aux false case.pattern with
              | true -> ok case
              | false ->
                let%bind body = substitute_var_in_body to_subst new_var case.body in
                ok { case with body }
            )
            m.cases
          in
          let m' = I.{matchee ; cases} in
          ret false { exp with content = I.E_matching m'}
        )
        | _ -> ret true exp
    in
    let%bind ((), res) = Self_ast_core.fold_map_expression aux () body in
    ok res

let make_var_pattern : O.expression_variable -> pattern =
  fun var -> Location.wrap @@ O.P_var { var ; ascr = None }

let rec partition : ('a -> bool) -> 'a list -> 'a list list =
  fun f lst ->
    let add_inner x ll =
      match ll with
      | hdl::tll -> (x::hdl)::tll
      | _ -> assert false
    in
    match lst with
    | [] -> []
    | [x] -> [[x]]
    | x::x'::tl ->
      if f x = f x' then add_inner x (partition f (x'::tl))
      else [x] :: (partition f (x'::tl))

let split_equations : equations -> equations O.label_map pm_result =
  fun eqs ->
    let aux : equations O.label_map -> typed_pattern list * (I.expression * O.environment) -> equations O.label_map pm_result =
      fun m (pl , (body , env)) ->
        let (phd,t) = List.hd pl in
        let ptl = List.tl pl in
        let dummy_p : unit -> typed_pattern = fun () ->
          let var =  Location.wrap @@ Var.fresh ~name:"_" () in
          (make_var_pattern var, O.t_unit ())
        in
        match phd.wrap_content with
        | P_variant (label,p_opt) ->
          let%bind t = extract_variant_type phd label t in
          let upd : equations option -> equations option = fun kopt ->
            match kopt, p_opt with
            | Some eqs , None   -> Some ( (dummy_p ()::ptl , (body,env))::eqs )
            | None     , None   -> Some [ (dummy_p ()::ptl , (body,env)) ]
            | Some eqs , Some p ->
              let p = (p,t) in
              Some (( p::ptl , (body,env))::eqs)
            | None     , Some p ->
              let p = (p,t) in
              Some [ (p::ptl          , (body,env)) ]
          in
          ok @@ O.LMap.update label upd m
        | _ -> corner_case __LOC__
    in
    bind_fold_right_list aux O.LMap.empty eqs

let rec match_ : type_f:type_fun -> body_t:O.type_expression option -> matchees -> equations -> rest -> O.expression pm_result =
  fun ~type_f ~body_t ms eqs def ->
  match ms , eqs with
  | [] , [([],(body,env))] ->
      let%bind body =
        type_f ?tv_opt:body_t env body in
      ok body
  | [] , eqs when List.for_all (fun (ps,_) -> List.length ps = 0) eqs ->
    let bodies = List.map (fun x -> fst (snd x)) eqs in
    fail @@ redundant_product_pattern bodies
  | _ ->
    let leq = partition (fun (pl,_) -> is_var (fst @@ List.hd pl)) eqs in
    let aux = fun (prev_opt:O.expression option) part_eq ->
      let%bind r =
        match prev_opt with
        | None -> consvar ~type_f ~body_t ms part_eq def
        | Some prev -> consvar ~type_f ~body_t ms part_eq prev.expression_content
      in
      ok (Some r)
    in
    let%bind r = bind_fold_right_list aux None leq in
    ok @@ Option.unopt_exn r

and consvar : type_f:type_fun -> body_t:O.type_expression option -> matchees -> equations -> rest -> O.expression pm_result =
  fun ~type_f ~body_t ms eqs def ->
  let p1s = List.map (fun el -> fst @@ List.hd @@ fst el) eqs in
    if List.for_all is_var p1s then
      let product_opt = is_product eqs in
      var_rule ~type_f ~body_t product_opt ms eqs def
    else
      ctor_rule ~type_f ~body_t ms eqs def

and var_rule : type_f:type_fun -> body_t:O.type_expression option -> typed_pattern option -> matchees -> equations -> rest -> O.expression pm_result =
  fun ~type_f ~body_t product_opt ms eqs def ->
  match ms with
  | mhd::mtl -> (
    match product_opt with
    | Some shape ->
      product_rule ~type_f ~body_t shape ms eqs def
    | None ->
      let aux : typed_pattern list * (I.expression * O.environment) -> (typed_pattern list * (I.expression * O.environment)) pm_result =
        fun (pl, (body,env)) ->
        match pl with
        | (phd,t)::ptl -> (
          match phd.wrap_content,t with
          | (P_var b, t) ->
            let%bind body' = substitute_var_in_body b.var mhd body in
            (* Is substitution avoidable ? mhd here can be the result of a tuple/record destructuring *)
            let env' = O.Environment.add_ez_binder mhd t env in
            ok (ptl , (body',env'))
          | (P_unit, _t) ->
            ok (ptl , (body,env))
          |  _ -> corner_case __LOC__
        )
        | [] -> corner_case __LOC__
      in
      let%bind eqs' = bind_map_list aux eqs in
      match_ ~type_f ~body_t mtl eqs' def
  )
  | [] -> corner_case __LOC__

and ctor_rule : type_f:type_fun -> body_t:O.type_expression option -> matchees -> equations -> rest -> O.expression pm_result =
  fun ~type_f ~body_t ms eqs def ->
  match ms with
  | mhd::mtl ->
    let aux_p : O.label * equations -> O.matching_content_case pm_result =
      fun (constructor,eq) ->
        let proj = Location.wrap @@ Var.fresh ~name:"ctor_proj" () in
        let new_ms = proj::mtl in
        let%bind nested = match_ ~type_f ~body_t new_ms eq def in
        ok @@ ({ constructor ; pattern = proj ; body = nested } : O.matching_content_case)
    in
    let aux_m : O.label * O.type_expression -> O.matching_content_case =
      fun (constructor,t) ->
        let proj = Location.wrap @@ Var.fresh ~name:"_" () in
        let body = O.make_e def t in
        { constructor ; pattern = proj ; body }
    in
    let%bind matchee_t = type_matchee eqs in
    let%bind eq_map = split_equations eqs in
    let%bind rows = trace_option (expected_variant Location.generated matchee_t) (O.get_t_sum matchee_t) in
    let eq_opt_map = O.LMap.mapi (fun label _ -> O.LMap.find_opt label eq_map) rows.content in
    let splitted_eqs = O.LMap.to_kv_list @@ eq_opt_map in
    let present = List.filter_map (fun (c,eq_opt) -> match eq_opt with Some eq -> Some (c,eq) | None -> None) splitted_eqs in
    let%bind present_cases = bind_map_list aux_p present in
    let matchee = O.make_e (O.e_variable mhd) matchee_t in
    let%bind body_t =
      let aux t_opt (c:O.matching_content_case) =
        let%bind () = assert_body_t ~body_t:t_opt c.body.location c.body.type_expression in
        match t_opt with
        | None -> ok (Some c.body.type_expression)
        | Some _ -> ok t_opt
      in
      let%bind t = bind_fold_list aux body_t present_cases in
      let t = Option.unopt_exn t in
      ok t
    in
    let missing = List.filter_map (fun (c,eq_opt) -> match eq_opt with Some _ -> None | None -> Some (c,body_t)) splitted_eqs in
    let missing_cases = List.map aux_m missing in
    let cases = O.Match_variant { cases = missing_cases @ present_cases ; tv = matchee_t } in
    ok @@ O.make_e (O.E_matching { matchee ; cases }) body_t
  | [] -> corner_case __LOC__

and product_rule : type_f:type_fun -> body_t:O.type_expression option -> typed_pattern -> matchees -> equations -> rest -> O.expression pm_result =
  fun ~type_f ~body_t product_shape ms eqs def ->
  match ms with
  | mhd::_ -> (
    let%bind lb =
      let (p,t) = product_shape in
      match (p.wrap_content,t) with
      | P_tuple ps , t ->
        let aux : int -> _ -> (O.label * (O.expression_variable * O.type_expression)) pm_result =
          fun i _ ->
            let l = (O.Label (string_of_int i)) in
            let%bind field_t = extract_record_type p l t in
            let v = Location.wrap @@ Var.fresh ~name:"tuple_proj" () in
            ok @@ (l, (v,field_t))
        in
        bind_mapi_list aux ps
      | P_record (labels,_) , t ->
        let aux : O.label -> (O.label * (O.expression_variable * O.type_expression)) pm_result  =
          fun l ->
            let v = Location.wrap @@ Var.fresh ~name:"record_proj" () in
            let%bind field_t = extract_record_type p l t in
            ok @@ (l , (v,field_t))
        in
        bind_map_list aux labels
      | _ -> corner_case __LOC__
    in
    let aux : typed_pattern list * (I.expression * O.environment) -> (typed_pattern list * (I.expression * O.environment)) pm_result =
      fun (pl, (body,env)) ->
      match pl with
      | (prod,t)::ptl -> (
        let var_filler = (make_var_pattern (Location.wrap @@ Var.fresh ~name:"_" ()) , t) in
        match prod.wrap_content with
        | P_tuple ps ->
          let aux i p =
            let%bind field_t = extract_record_type p (O.Label (string_of_int i)) t in
            ok (p,field_t)
          in
          let%bind tps = bind_mapi_list aux ps in
          ok (tps @ var_filler::ptl , (body,env))
        | P_record (labels,ps) ->
          let aux (label,p) =
            let%bind field_t = extract_record_type p label t in
            ok (p,field_t)
          in
          let%bind tps = bind_map_list aux (List.combine labels ps) in
          ok (tps @ var_filler::ptl , (body,env))
        | P_var _ ->
          let%bind filler =
            let (p,t) = product_shape in
            match (p.wrap_content,t) with
            | P_tuple ps , t ->
              let aux i _ =
                let%bind field_t = extract_record_type p (O.Label (string_of_int i)) t in
                let v = make_var_pattern (Location.wrap @@ Var.fresh ~name:"_" ()) in
                ok (v,field_t)
              in
              bind_mapi_list aux ps
            | P_record (labels,_) , t ->
              let aux l =
                let%bind field_t = extract_record_type p l t in
                let v = make_var_pattern (Location.wrap @@ Var.fresh ~name:"_" ()) in
                ok (v,field_t)
              in
              bind_map_list aux labels
            | _ -> corner_case __LOC__
          in
          ok (filler @ pl , (body,env))
        | _ -> corner_case __LOC__
      )
      | [] -> corner_case __LOC__
    in
    let%bind matchee_t = type_matchee eqs in
    let%bind eqs' = bind_map_list aux eqs in
    let fields = O.LMap.of_list lb in
    let new_matchees = List.map (fun (_,((x:O.expression_variable),_)) -> x) lb in
    let%bind body = match_ ~type_f ~body_t (new_matchees @ ms) eqs' def in
    let cases = O.Match_record { fields; body ; tv = snd product_shape } in
    let matchee = O.make_e (O.e_variable mhd) matchee_t in
    ok @@ O.make_e (O.E_matching { matchee ; cases }) body.type_expression
  )
  | [] -> corner_case __LOC__

and compile_matching ~type_f ~body_t matchee (eqs:equations) =
    let f =
      match is_product eqs with
      | Some shape -> product_rule ~type_f ~body_t shape
      | None -> match_ ~type_f ~body_t
    in
    let def =
      let fs = O.make_e (O.E_literal (O.Literal_string Stage_common.Backends.fw_partial_match)) (O.t_string ()) in
      O.e_failwith fs
    in
    f [matchee] eqs def