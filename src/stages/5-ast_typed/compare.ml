open Ast
open Compare_enum

let cmp2 f a1 b1 g a2 b2 = match f a1 b1 with 0 -> g a2 b2 | c -> c
let cmp3 f a1 b1 g a2 b2 h a3 b3 = match f a1 b1 with 0 -> (match g a2 b2 with 0 -> h a3 b3 | c -> c) | c -> c
let cmp4 f a1 b1 g a2 b2 h a3 b3 i a4 b4 = match f a1 b1 with 0 -> (match g a2 b2 with 0 -> (match h a3 b3 with 0 -> i a4 b4 | c -> c) | c -> c) | c -> c
let cmp5 f a1 b1 g a2 b2 h a3 b3 i a4 b4 j a5 b5 = match f a1 b1 with 0 -> (match g a2 b2 with 0 -> (match h a3 b3 with 0 -> (match i a4 b4 with 0 -> j a5 b5 | c -> c) | c -> c) | c -> c) | c -> c
let cmp6 f a1 b1 g a2 b2 h a3 b3 i a4 b4 j a5 b5 k a6 b6 = match f a1 b1 with 0 -> (match g a2 b2 with 0 -> (match h a3 b3 with 0 -> (match i a4 b4 with 0 -> (match j a5 b5 with 0 -> k a6 b6 | c -> c) | c -> c) | c -> c) | c -> c) | c -> c

let cmp_pair f g (a1, a2) (b1, b2) = cmp2 f a1 b1 g a2 b2

let compare_lmap_entry  compare (Label na, va) (Label nb, vb) = cmp2 String.compare na nb compare va vb
let compare_tvmap_entry compare (tva, va) (tvb, vb) = cmp2 Var.compare tva tvb compare va vb

let bool a b = (Stdlib.compare : bool -> bool -> int) a b
let label (Label a) (Label b) = String.compare a b
let label_map ~compare lma lmb =
  let ra = LMap.to_kv_list_rev lma in
  let rb = LMap.to_kv_list_rev lmb in
  let aux (la,a) (lb,b) =
    cmp2 label la lb compare a b in
  List.compare ~compare:aux ra rb

let typeVariableMap compare a b = List.compare ~compare:(compare_tvmap_entry compare) a b

let expression_variable = Location.compare_wrap ~compare:Var.compare
let type_variable       = Var.compare
let module_variable     = String.compare

let module_access f {module_name=mna; element=ea}
                    {module_name=mnb; element=eb} =
  cmp2
    module_variable mna mnb
    f ea eb

let layout_tag = function
  | L_comb -> 1
  | L_tree -> 2

let layout a b = Int.compare (layout_tag a) (layout_tag b)

let type_expression_tag ty_cont =
  match ty_cont with
    T_variable        _ -> 1
  | T_constant        _ -> 2
  | T_sum             _ -> 3
  | T_record          _ -> 4
  | T_arrow           _ -> 5
  | T_module_accessor _ -> 6
  | T_singleton       _ -> 7

let rec constant_tag (ct : constant_tag) =
  match ct with
    C_arrow     ->  1
  | C_option    ->  2
  | C_map       ->  3
  | C_big_map   ->  4
  | C_list      ->  5
  | C_set       ->  6
  | C_unit      ->  8
  | C_string    ->  7
  | C_nat       ->  9
  | C_mutez     -> 10
  | C_timestamp -> 11
  | C_int       -> 12
  | C_address   -> 13
  | C_bytes     -> 14
  | C_key_hash  -> 15
  | C_key       -> 16
  | C_signature -> 17
  | C_operation -> 18
  | C_contract  -> 19
  | C_chain_id  ->  0

and type_expression a b =
  type_content a.type_content b.type_content

and type_content a b =
  match a, b with
    T_variable a, T_variable b -> type_variable a b
  | T_constant a, T_constant b -> injection a b
  | T_sum      a, T_sum      b -> rows a b
  | T_record   a, T_record   b -> rows a b
  | T_arrow    a, T_arrow    b -> arrow a b
  | T_module_accessor a, T_module_accessor b -> module_access type_expression a b
  | T_singleton a , T_singleton b -> literal a b
  | (T_variable _| T_constant _| T_sum _| T_record _| T_arrow _ | T_module_accessor _ | T_singleton _),
    (T_variable _| T_constant _| T_sum _| T_record _| T_arrow _ | T_module_accessor _ | T_singleton _) ->
    Int.compare (type_expression_tag a) (type_expression_tag b)

and injection {language=la ; injection=ia ; parameters=pa} {language=lb ; injection=ib ; parameters=pb} =
  cmp3
    String.compare la lb
    Ligo_string.compare ia ib
    (List.compare ~compare:type_expression) pa pb

and rows {content=ca; layout=la} {content=cb; layout=lb} =
  cmp2
    (label_map ~compare:row_element) ca cb
    layout la lb

and constraint_identifier (ConstraintIdentifier a) (ConstraintIdentifier b) =
  cmp2
    Int64.compare a b
    (List.compare ~compare:type_expression) [] []

and constraint_identifier_set (a : constraint_identifier PolySet.t) (b : constraint_identifier PolySet.t) : int =
  List.compare ~compare:constraint_identifier (PolySet.elements a)  (PolySet.elements b)

and row_element {associated_type=aa;michelson_annotation=ma;decl_pos=da} {associated_type=ab;michelson_annotation=mb;decl_pos=db} =
  cmp3
    type_expression aa ab
    (Option.compare String.compare) ma mb
    Int.compare     da db

and arrow {type1=ta1;type2=tb1} {type1=ta2;type2=tb2} =
  cmp2
    type_expression ta1 ta2
    type_expression tb1 tb2

let constant_tag (ct : constant_tag) (ct2 : constant_tag) =
  Int.compare (constant_tag ct ) (constant_tag ct2 )

let binder (va,ta) (vb,tb) =
  cmp2 expression_variable va vb type_expression ta tb

let expression_tag expr =
  match expr.expression_content with
    E_literal         _ -> 1
  | E_constant        _ -> 2
  | E_variable        _ -> 3
  | E_application     _ -> 4
  | E_lambda          _ -> 5
  | E_recursive       _ -> 6
  | E_let_in          _ -> 7
  | E_type_in         _ -> 8
  | E_mod_in          _ -> 9
  | E_mod_alias       _ -> 10
  | E_raw_code        _ -> 11
  (* Variant *)
  | E_constructor     _ -> 12
  | E_matching        _ -> 13
  (* Record *)
  | E_record          _ -> 14
  | E_record_accessor _ -> 15
  | E_record_update   _ -> 16
  | E_module_accessor _ -> 17

and declaration_tag = function
  | Declaration_constant _ -> 1
  | Declaration_type     _ -> 2
  | Declaration_module   _ -> 3
  | Module_alias         _ -> 4

let rec expression a b =
  match a.expression_content,b.expression_content with
    E_literal  a, E_literal  b -> compare a b
  | E_constant a, E_constant b -> constant a b
  | E_variable a, E_variable b -> expression_variable a b
  | E_application a, E_application b -> application a b
  | E_lambda a, E_lambda b -> lambda a b
  | E_recursive a, E_recursive b -> recursive a b
  | E_let_in a, E_let_in b -> let_in a b
  | E_type_in a, E_type_in b -> type_in a b
  | E_mod_in a, E_mod_in b -> mod_in a b
  | E_mod_alias a, E_mod_alias b -> mod_alias a b
  | E_raw_code a, E_raw_code b -> raw_code a b
  | E_constructor a, E_constructor b -> constructor a b
  | E_matching a, E_matching b -> matching a b
  | E_record a, E_record b -> record a b
  | E_record_accessor a, E_record_accessor b -> record_accessor a b
  | E_record_update  a, E_record_update b -> record_update a b
  | E_module_accessor a, E_module_accessor b -> module_access expression a b
  | (E_literal _| E_constant _| E_variable _| E_application _| E_lambda _| E_recursive _| E_let_in _| E_type_in _| E_mod_in _| E_mod_alias _| E_raw_code _| E_constructor _| E_matching _| E_record _| E_record_accessor _| E_record_update _ | E_module_accessor _),
    (E_literal _| E_constant _| E_variable _| E_application _| E_lambda _| E_recursive _| E_let_in _| E_type_in _| E_mod_in _| E_mod_alias _| E_raw_code _| E_constructor _| E_matching _| E_record _| E_record_accessor _| E_record_update _ | E_module_accessor _) ->
    Int.compare (expression_tag a) (expression_tag b)

and constant ({cons_name=ca;arguments=a}: constant) ({cons_name=cb;arguments=b}: constant) =
  cmp2 constant' ca cb (List.compare ~compare:expression) a b

and application ({lamb=la;args=a}) ({lamb=lb;args=b}) =
  cmp2 expression la lb expression a b

and lambda ({binder=ba;result=ra}) ({binder=bb;result=rb}) =
  cmp2 expression_variable ba bb expression ra rb

and recursive ({fun_name=fna;fun_type=fta;lambda=la}) {fun_name=fnb;fun_type=ftb;lambda=lb} =
  cmp3
    expression_variable fna fnb
    type_expression     fta ftb
    lambda               la  lb

and let_in {let_binder=ba;rhs=ra;let_result=la;inline=aa} {let_binder=bb;rhs=rb;let_result=lb;inline=ab} =
  cmp4
    expression_variable ba bb
    expression ra rb
    expression la lb
    bool  aa ab

and type_in {type_binder=ba;rhs=ra;let_result=la} {type_binder=bb;rhs=rb;let_result=lb} =
  cmp3
    type_variable ba bb
    type_expression ra rb
    expression la lb

and mod_in {module_binder=ba;rhs= Module_Fully_Typed ra;let_result=la} {module_binder=bb;rhs= Module_Fully_Typed rb;let_result=lb} =
  cmp3
    module_variable ba bb
    module_ ra rb
    expression la lb

and mod_alias {alias=aa; binders=ba; result=la} {alias=ab; binders=bb; result=lb} =
  cmp3
    module_variable aa ab
    (List.Ne.compare ~compare:module_variable) ba bb
    expression la lb

and raw_code {language=la;code=ca} {language=lb;code=cb} =
  cmp2
    String.compare la lb
    expression     ca cb

and constructor {constructor=ca;element=ea} {constructor=cb;element=eb} =
  cmp2
    label ca cb
    expression ea eb

and matching {matchee=ma;cases=ca} {matchee=mb;cases=cb} =
  cmp2
    expression ma mb
    matching_expr ca cb

and record ra rb = label_map ~compare:expression ra rb

and record_accessor {record=ra;path=pa} {record=rb;path=pb} =
  cmp2
    expression ra rb
    label pa pb

and record_update {record=ra;path=pa;update=ua} {record=rb;path=pb;update=ub} =
  cmp3
    expression ra rb
    label pa pb
    expression ua ub

and matching_expr_tag = function
  Match_list    _ -> 1
| Match_option  _ -> 2
| Match_variant _ -> 3
| Match_record _ -> 4

and matching_expr a b =
  match (a,b) with
    Match_list    a, Match_list    b -> matching_content_list a b
  | Match_option  a, Match_option  b -> matching_content_option a b
  | Match_variant a, Match_variant b -> matching_content_variant a b
  | Match_record a, Match_record b -> matching_content_record a b
  | (Match_list _| Match_option _| Match_variant _ | Match_record _),
    (Match_list _| Match_option _| Match_variant _ | Match_record _) ->
    Int.compare (matching_expr_tag a) (matching_expr_tag b)

and matching_content_cons {hd=ha;tl=ta;body=ba;tv=va} {hd=hb;tl=tb;body=bb;tv=vb} =
  cmp4
    expression_variable ha hb
    expression_variable ta tb
    expression      ba bb
    type_expression va vb

and matching_content_list {match_nil=na;match_cons=ca} {match_nil=nb;match_cons=cb} =
  cmp2
    expression na nb
    matching_content_cons ca cb

and matching_content_some {opt=oa;body=ba;tv=ta} {opt=ob;body=bb;tv=tb} =
  cmp3
    expression_variable oa ob
    expression ba bb
    type_expression ta tb

and matching_content_option {match_none=na;match_some=sa} {match_none=nb;match_some=sb} =
  cmp2
    expression na nb
    matching_content_some sa sb

and matching_content_case {constructor=ca;pattern=pa;body=ba} {constructor=cb;pattern=pb;body=bb} =
  cmp3
    label ca cb
    expression_variable pa pb
    expression ba bb

and matching_content_variant {cases=ca;tv=ta} {cases=cb;tv=tb} =
  cmp2
    (List.compare ~compare:matching_content_case) ca cb
    type_expression ta tb

and matching_content_record
    {fields = fields1; body = body1; record_type = t1}
    {fields = fields2; body = body2; record_type = t2} =
  cmp3
    (label_map ~compare:(cmp_pair expression_variable type_expression)) fields1 fields2
    expression body1 body2
    rows t1 t2

and declaration_constant {name=na;binder=ba;expr=ea;inline=ia} {name=nb;binder=bb;expr=eb;inline=ib} =
  cmp4
    (Option.compare String.compare) na nb
    expression_variable ba bb
    expression ea eb
    bool ia ib

and declaration_type {type_binder=tba;type_expr=tea} {type_binder=tbb;type_expr=teb} =
  cmp2
    type_variable tba tbb
    type_expression tea teb

and declaration_module {module_binder=mba;module_= Module_Fully_Typed ma} {module_binder=mbb;module_= Module_Fully_Typed mb} =
 cmp2
    module_variable mba mbb
    module_ ma mb

and module_alias : module_alias -> module_alias -> int
= fun {alias = aa; binders = ba} {alias = ab; binders = bb} ->
 cmp2
    module_variable aa ab
    (List.Ne.compare ~compare:module_variable) ba bb

and declaration a b =
  match (a,b) with
    Declaration_constant a, Declaration_constant b -> declaration_constant a b
  | Declaration_type     a, Declaration_type     b -> declaration_type a b
  | Declaration_module   a, Declaration_module   b -> declaration_module a b
  | Module_alias         a, Module_alias         b -> module_alias a b
  | (Declaration_constant _| Declaration_type _| Declaration_module _| Module_alias _),
    (Declaration_constant _| Declaration_type _| Declaration_module _| Module_alias _) ->
    Int.compare (declaration_tag a) (declaration_tag b)

and module_ m = List.compare ~compare:(Location.compare_wrap ~compare:declaration) m

(* Environment *)
let free_variables = List.compare ~compare:expression_variable

let type_environment_binding {type_variable=va;type_=ta} {type_variable=vb;type_=tb} =
  cmp2
    type_variable va vb
    type_expression ta tb

let type_environment = List.compare ~compare:type_environment_binding

let environment_element_definition_declaration {expression=ea;free_variables=fa} {expression=eb;free_variables=fb} =
  cmp2
    expression ea eb
    free_variables fa fb

let environment_element_definition a b = match a,b with
  | ED_binder, ED_declaration _ -> -1
  | ED_binder, ED_binder -> 0
  | ED_declaration _, ED_binder -> 1
  | ED_declaration a, ED_declaration b -> environment_element_definition_declaration a b

let rec environment_element {type_value=ta;source_environment=sa;definition=da} {type_value=tb;source_environment=sb;definition=db} =
  cmp3
    type_expression ta tb
    environment sa sb
    environment_element_definition da db

and environment_binding {expr_var=eva;env_elt=eea} {expr_var=evb;env_elt=eeb} =
  cmp2
    expression_variable eva evb
    environment_element eea eeb

and expression_environment a b = List.compare ~compare:environment_binding a b

and module_environment_binding {module_variable=mva;module_=ma}
                               {module_variable=mvb;module_=mb} =
  cmp2
    module_variable mva mvb
    environment    ma  mb

and module_environment a b = List.compare ~compare:module_environment_binding a b

and environment {expression_environment=eea;type_environment=tea; module_environment=mea}
                {expression_environment=eeb;type_environment=teb; module_environment=meb} =
  cmp3
   expression_environment eea eeb
   type_environment       tea teb
   module_environment     mea meb

(* Solver types *)

let unionfind a b =
  let a = UnionFind.Poly2.partitions a in
  let b = UnionFind.Poly2.partitions b in
  List.compare ~compare:(List.compare ~compare:type_variable) a b

let type_constraint_tag = function
  | C_equation     _ -> 1
  | C_typeclass    _ -> 2
  | C_access_label _ -> 3

let type_value_tag = function
  | P_forall   _  -> 1
  | P_variable _  -> 2
  | P_constant _  -> 3
  | P_apply    _  -> 4
  | P_row      _  -> 5


let row_tag = function
  | C_record  -> 1
  | C_variant -> 2

let row_tag a b = Int.compare (row_tag a) (row_tag b)

let rec type_value_ a b = match (a,b) with
  P_forall   a, P_forall   b -> p_forall a b
| P_variable a, P_variable b -> type_variable a b
| P_constant a, P_constant b -> p_constant a b
| P_apply    a, P_apply    b -> p_apply a b
| P_row      a, P_row      b -> p_row a b
| a, b -> Int.compare (type_value_tag a) (type_value_tag b)

and type_value : type_value_ location_wrap -> type_value_ location_wrap -> int = fun ta tb ->
    type_value_ ta.wrap_content tb.wrap_content

and p_constraints c = List.compare ~compare:type_constraint c

and p_forall {binder=ba;constraints=ca;body=a} {binder=bb;constraints=cb;body=b} =
  cmp3
    type_variable ba bb
    p_constraints ca cb
    type_value    a  b

and p_constant {p_ctor_tag=ca;p_ctor_args=la} {p_ctor_tag=cb;p_ctor_args=lb} =
  cmp2
    constant_tag ca cb
    (List.compare ~compare:type_value) la lb

and p_apply {tf=ta;targ=la} {tf=tb;targ=lb} =
  cmp2
    type_value ta tb
    type_value la lb

and p_row {p_row_tag=ra;p_row_args=la} {p_row_tag=rb;p_row_args=lb} =
  cmp2
    row_tag ra rb
    (label_map ~compare:row_value) la lb

and row_value {associated_value=aa;michelson_annotation=ma;decl_pos=da} {associated_value=ab;michelson_annotation=mb;decl_pos=db} =
  cmp3
    type_value aa ab
    (Option.compare String.compare) ma mb
    Int.compare     da db

and type_constraint {reason=ra;c=ca} {reason=rb;c=cb} =
  cmp2
    String.compare   ra rb
    type_constraint_ ca cb

and type_constraint_ a b = match (a,b) with
  | C_equation     a, C_equation     b -> c_equation a b
  | C_typeclass    a, C_typeclass    b -> c_typeclass a b
  | C_access_label a, C_access_label b -> c_access_label a b
  | a, b -> Int.compare (type_constraint_tag a) (type_constraint_tag b)

and c_equation {aval=a1;bval=b1} {aval=a2;bval=b2} =
  cmp2
    type_value a1 a2
    type_value b1 b2

and tc_args a = List.compare ~compare:type_value a

and c_typeclass {tc_args=ta;typeclass=ca; original_id=x} {tc_args=tb;typeclass=cb; original_id =y} =
  cmp3
    tc_args ta tb
    typeclass ca cb
    (Option.compare (constraint_identifier ))x y

and c_access_label
      {c_access_label_tval=val1;accessor=a1;c_access_label_tvar=var1}
      {c_access_label_tval=val2;accessor=a2;c_access_label_tvar=var2} =
  cmp3
    type_value val1 val2
    label a1 a2
    type_variable var1 var2

and tc_allowed t = List.compare ~compare:type_value t
and typeclass  t = List.compare ~compare:tc_allowed t

let c_constructor_simpl {id_constructor_simpl = ConstraintIdentifier ca;_} {id_constructor_simpl = ConstraintIdentifier cb;_} =
  Int64.compare ca cb

let c_alias {reason_alias_simpl=ra;a=aa;b=ba} {reason_alias_simpl=rb;a=ab;b=bb} =
  cmp3
    String.compare ra rb
    type_variable  aa ab
    type_variable  ba bb

let c_poly_simpl {id_poly_simpl = ConstraintIdentifier ca;_} {id_poly_simpl = ConstraintIdentifier cb;_} =
  Int64.compare ca cb

(* let c_typeclass_simpl {reason_typeclass_simpl=ra;id_typeclass_simpl=ida;original_id=oia;tc=ta;args=la} {reason_typeclass_simpl=rb;id_typeclass_simpl=idb;original_id=oib;tc=tb;args=lb} =
 *   cmp6
 *     String.compare ra rb
 *     Bool.compare imca imcb
 *     constraint_identifier ida idb
 *     (Option.compare constraint_identifier) oia oib
 *     (List.compare ~compare:tc_allowed) ta tb
 *     (List.compare ~compare:type_variable) la lb *)

let c_typeclass_simpl a b =
  constraint_identifier a.id_typeclass_simpl b.id_typeclass_simpl

let c_access_label_simpl a b =
  constraint_identifier a.id_access_label_simpl b.id_access_label_simpl

let c_row_simpl {id_row_simpl = ConstraintIdentifier ca;_} {id_row_simpl = ConstraintIdentifier cb;_} =
  Int64.compare ca cb

let constructor_or_row
    (a : constructor_or_row)
    (b : constructor_or_row) =
  match a,b with
  | `Row a , `Row b -> c_row_simpl a b
  | `Constructor a , `Constructor b -> c_constructor_simpl a b
  | `Constructor _ , `Row _ -> -1
  | `Row _ , `Constructor _ -> 1

let type_constraint_simpl_tag = function
  | SC_Constructor _ -> 1
  | SC_Alias       _ -> 2
  | SC_Poly        _ -> 3
  | SC_Typeclass   _ -> 4
  | SC_Access_label   _ -> 6
  | SC_Row         _ -> 5

let type_constraint_simpl a b =
  match (a,b) with
  SC_Constructor ca, SC_Constructor cb -> c_constructor_simpl ca cb
| SC_Alias       aa, SC_Alias       ab -> c_alias aa ab
| SC_Poly        pa, SC_Poly        pb -> c_poly_simpl pa pb
| SC_Typeclass   ta, SC_Typeclass   tb -> c_typeclass_simpl ta tb
| SC_Access_label la, SC_Access_label lb -> c_access_label_simpl la lb
| SC_Row         ra, SC_Row         rb -> c_row_simpl ra rb
| a, b -> Int.compare (type_constraint_simpl_tag a) (type_constraint_simpl_tag b)

let deduce_and_clean_result {deduced=da;cleaned=ca} {deduced=db;cleaned=cb} =
  cmp2
    (List.compare ~compare:c_constructor_simpl) da db
    c_typeclass_simpl ca cb
