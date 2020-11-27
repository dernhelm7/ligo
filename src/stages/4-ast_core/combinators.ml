open Types
module Option = Simple_utils.Option

module SMap = Map.String
open Stage_common.Constant

let make_t ?(loc = Location.generated) ?sugar type_content = ({type_content; sugar; location=loc}: type_expression)

let tuple_to_record lst =
  let aux (i,acc) el = (i+1,(string_of_int i, el)::acc) in
  let (_, lst ) = List.fold_left aux (0,[]) lst in
  lst

let t_variable ?loc ?sugar variable : type_expression = make_t ?loc ?sugar @@ T_variable variable
let t_app ?loc ?sugar type_operator arguments : type_expression = make_t ?loc ?sugar @@ T_app {type_operator ; arguments}


let t_bool      ?loc ?sugar () : type_expression = t_variable ?loc ?sugar v_bool
let t_string    ?loc ?sugar () : type_expression = t_variable ?loc ?sugar v_string
let t_bytes     ?loc ?sugar () : type_expression = t_variable ?loc ?sugar v_bytes
let t_int       ?loc ?sugar () : type_expression = t_variable ?loc ?sugar v_int
let t_operation ?loc ?sugar () : type_expression = t_variable ?loc ?sugar v_operation
let t_nat       ?loc ?sugar () : type_expression = t_variable ?loc ?sugar v_nat
let t_tez       ?loc ?sugar () : type_expression = t_variable ?loc ?sugar v_tez
let t_unit      ?loc ?sugar () : type_expression = t_variable ?loc ?sugar v_unit
let t_address   ?loc ?sugar () : type_expression = t_variable ?loc ?sugar v_address
let t_signature ?loc ?sugar () : type_expression = t_variable ?loc ?sugar v_signature
let t_key       ?loc ?sugar () : type_expression = t_variable ?loc ?sugar v_key
let t_key_hash  ?loc ?sugar () : type_expression = t_variable ?loc ?sugar v_key_hash
let t_timestamp ?loc ?sugar () : type_expression = t_variable ?loc ?sugar v_timestamp
let t_option    ?loc ?sugar o  : type_expression = t_app ?loc ?sugar v_option [o]
let t_list      ?loc ?sugar t  : type_expression = t_app ?loc ?sugar v_list [t]
let t_record_ez ?loc ?sugar ?layout lst =
  let lst = List.map (fun (k, v) -> (Label k, v)) lst in
  let m = LMap.of_list lst in
  make_t ?loc ?sugar @@ T_record { layout ; fields = m }
let t_record ?loc ?sugar m  : type_expression =
  let lst = SMap.to_kv_list_rev m in
  t_record_ez ?loc ?sugar lst

let t_pair  ?loc ?sugar (a , b) : type_expression = t_record_ez ?loc ?sugar [("0",a) ; ("1",b)]
let t_tuple ?loc ?sugar lst     : type_expression = t_record_ez ?loc ?sugar (tuple_to_record lst)

let ez_t_sum ?loc ?sugar ?layout (lst:(string * row_element) list) : type_expression =
  let lst = List.map (fun (k, v) -> (Label k, v)) lst in
  let m = LMap.of_list lst in
  make_t ?loc ?sugar @@ T_sum { layout ; fields = m }
let t_sum ?loc ?sugar m : type_expression =
  let lst = SMap.to_kv_list_rev m in
  ez_t_sum ?loc ?sugar lst

let t_function ?loc ?sugar type1 type2  : type_expression = make_t ?loc ?sugar @@ T_arrow {type1; type2}
let t_map      ?loc ?sugar key value : type_expression = t_app ?loc ?sugar (v_map) [key ; value]
let t_big_map  ?loc ?sugar key value : type_expression = t_app ?loc ?sugar (v_big_map) [key ; value]
let t_set      ?loc ?sugar t         : type_expression = t_app ?loc ?sugar (v_set) [t]
let t_contract ?loc ?sugar t         : type_expression = t_app ?loc ?sugar (v_contract) [t]

let make_e ?(loc = Location.generated) ?sugar content = {content; sugar; location=loc }

let e_var       ?loc ?sugar n  : expression = make_e ?loc ?sugar @@ E_variable (Location.wrap ?loc (Var.of_name n))
let e_literal   ?loc ?sugar l  : expression = make_e ?loc ?sugar @@ E_literal l
let e_unit      ?loc ?sugar () : expression = make_e ?loc ?sugar @@ E_literal (Literal_unit)
let e_int       ?loc ?sugar n  : expression = make_e ?loc ?sugar @@ E_literal (Literal_int n)
let e_nat       ?loc ?sugar n  : expression = make_e ?loc ?sugar @@ E_literal (Literal_nat n)
let e_timestamp ?loc ?sugar n  : expression = make_e ?loc ?sugar @@ E_literal (Literal_timestamp n)
let e_string    ?loc ?sugar s  : expression = make_e ?loc ?sugar @@ E_literal (Literal_string s)
let e_address   ?loc ?sugar s  : expression = make_e ?loc ?sugar @@ E_literal (Literal_address s)
let e_mutez     ?loc ?sugar s  : expression = make_e ?loc ?sugar @@ E_literal (Literal_mutez s)
let e_signature ?loc ?sugar s  : expression = make_e ?loc ?sugar @@ E_literal (Literal_signature s)
let e_key       ?loc ?sugar s  : expression = make_e ?loc ?sugar @@ E_literal (Literal_key s)
let e_key_hash  ?loc ?sugar s  : expression = make_e ?loc ?sugar @@ E_literal (Literal_key_hash s)
let e_chain_id  ?loc ?sugar s  : expression = make_e ?loc ?sugar @@ E_literal (Literal_chain_id s)
let e'_bytes b : expression_content =
  let bytes = Hex.to_bytes (`Hex b) in
  E_literal (Literal_bytes bytes)
let e_bytes_hex ?loc ?sugar b : expression =
  let e' = e'_bytes b in
  make_e ?loc ?sugar e'
let e_bytes_raw ?loc ?sugar (b: bytes) : expression =
  make_e ?loc ?sugar @@ E_literal (Literal_bytes b)
let e_bytes_string ?loc ?sugar (s: string) : expression =
  make_e ?loc ?sugar @@ E_literal (Literal_bytes (Hex.to_bytes (Hex.of_string s)))
let e_variable ?loc ?sugar v = make_e ?loc ?sugar @@ E_variable v
let e_application ?loc ?sugar a b                                  = make_e ?loc ?sugar @@ E_application {lamb=a ; args=b}
let e_some ?loc s  : expression = make_e ?loc @@
  E_constructor {constructor = Label Stage_common.Constant.ctor_some_name; element = s}
let e_none ?loc () : expression = make_e ?loc @@
  E_constructor {constructor = Label Stage_common.Constant.ctor_none_name; element = e_unit ()}
let e_lambda      ?loc ?sugar binder output_type result            = make_e ?loc ?sugar @@ E_lambda {binder; output_type; result ;  }
let e_lambda_ez   ?loc ?sugar var ?ascr output_type result         = e_lambda ?loc ?sugar {var;ascr} output_type result
let e_recursive   ?loc ?sugar fun_name fun_type lambda             = make_e ?loc ?sugar @@ E_recursive {fun_name; fun_type; lambda}
let e_let_in      ?loc ?sugar let_binder inline rhs let_result     = make_e ?loc ?sugar @@ E_let_in { let_binder ; rhs ; let_result; inline }
let e_let_in_ez   ?loc ?sugar var ?ascr  inline rhs let_result     = e_let_in ?loc ?sugar {var;ascr} inline rhs let_result
let e_raw_code    ?loc ?sugar language code                        = make_e ?loc ?sugar @@ E_raw_code {language; code}

let e_constructor ?loc ?sugar s a : expression = make_e ?loc ?sugar @@ E_constructor { constructor = Label s; element = a}
let e_matching    ?loc ?sugar a b : expression = make_e ?loc ?sugar @@ E_matching {matchee=a;cases=b}

let e_record          ?loc ?sugar map = make_e ?loc ?sugar @@ E_record map
let e_record_accessor ?loc ?sugar record path        = make_e ?loc ?sugar @@ E_record_accessor ({record; path} : _ record_accessor)
let e_record_update   ?loc ?sugar record path update = make_e ?loc ?sugar @@ E_record_update ({record; path; update} : _ record_update)
let e_record_ez ?loc ?sugar kvl =
  let rec aux i x =
    match x with
    | hd::tl -> (Label (string_of_int i) , hd) :: aux (i+1) tl
    | [] -> []
  in
  e_record ?loc ?sugar (LMap.of_list (aux 0 kvl))
let constant_app ?loc name args =
  let lamb = e_variable name in
  let args = e_record_ez args in
  e_application ?loc lamb args
let e_annotation ?loc ?sugar anno_expr ty = make_e ?loc ?sugar @@ E_ascription {anno_expr; type_annotation = ty}

let e_bool ?loc ?sugar b : expression = e_constructor ?loc ?sugar (string_of_bool b) (e_unit ())

let make_option_typed ?loc ?sugar e t_opt =
  match t_opt with
  | None -> e
  | Some t -> e_annotation ?loc ?sugar e t

let e_typed_none ?loc t_opt =
  let type_annotation = t_option t_opt in
  e_annotation ?loc (e_none ?loc ()) type_annotation



let get_e_record_accessor = fun t ->
  match t with
  | E_record_accessor {record; path} -> Some (record, path)
  | _ -> None

let assert_e_record_accessor = fun t ->
  match get_e_record_accessor t with
  | Some _ -> Some ()
  | None -> None

let get_e_pair = fun t ->
  match t with
  | E_record r -> (
  let lst = LMap.to_kv_list_rev r in
    match lst with
    | [(Label "O",a);(Label "1",b)]
    | [(Label "1",b);(Label "0",a)] ->
        Some (a , b)
    | _ -> None
    )
  | _ -> None

let get_e_list = fun t ->
  let open Stage_common.Constant in
  let rec aux t =
    match t with
    | E_application {lamb;args} -> (
      match lamb.content, args.content with
      | E_variable v , E_record x when Var.equal v.wrap_content ev_cons.wrap_content -> (
        match LMap.to_list x with
        | [ key ; lst ] ->
          let lst = aux lst.content in
          (Some key)::(lst)
        | _ -> [None]
      )
      | E_variable v , E_record x when Var.equal v.wrap_content ev_list_empty.wrap_content && LMap.cardinal x = 0 -> []
      | _ -> [None]
    )
    | _ -> [None]
  in
  let opts = aux t in
  if List.exists (Option.is_none) opts then None
  else Some (List.map Option.unopt_exn opts)

let get_e_tuple = fun t ->
  match t with
  | E_record r -> Some (List.map snd @@ Helpers.tuple_of_record r)
  | _ -> None

let get_e_ascription = fun a ->
  match a with
  | E_ascription {anno_expr; type_annotation} -> Some (anno_expr,type_annotation)
  | _ -> None

(* Same as get_e_pair *)
let extract_pair : expression -> (expression * expression) option = fun e ->
  match e.content with
  | E_record r -> (
  let lst = LMap.to_kv_list_rev r in
    match lst with
    | [(Label "O",a);(Label "1",b)]
    | [(Label "1",b);(Label "0",a)] ->
      Some (a , b)
    | _ -> None
    )
  | _ -> None

let extract_record : expression -> (label * expression) list option = fun e ->
  match e.content with
  | E_record lst -> Some (LMap.to_kv_list lst)
  | _ -> None

let extract_map : expression -> (expression * expression) list option = fun e ->
  let open Stage_common.Constant in
  let rec aux e =
    match e.content with
    | E_application {lamb ; args} -> (
      match lamb.content, args.content with
      | E_variable v , E_record x when Var.equal v.wrap_content ev_map_add.wrap_content || Var.equal v.wrap_content ev_update.wrap_content -> (
        match LMap.to_list x with
        | [ k ; v ; map ] ->
          let map = aux map in
          (Some (k,v))::map
        | _ -> [None]
      )
      | E_variable v , E_record x when Var.equal v.wrap_content ev_big_map_empty.wrap_content || Var.equal v.wrap_content ev_map_empty.wrap_content -> (
        match LMap.to_list x with
          | [] -> []
          | _ -> [None]
      )
      | _ -> [None]
    )
    | _ -> [None]
  in
  let opts = aux e in
  if List.exists (Option.is_none) opts then None
  else Some (List.map Option.unopt_exn opts)
