open Ast_typed
open Format
module UF = UnionFind.Poly2

let type_constraint_ : _ -> type_constraint_simpl_ -> unit = fun ppf ->
  function
  |SC_Constructor { tv; c_tag; tv_list=_ } ->
    let ct = match c_tag with
      | C_arrow        -> "arrow"
      | C_option       -> "option"
      | C_record       -> failwith "record"
      | C_variant      -> failwith "variant"
      | C_map          -> "map"
      | C_big_map      -> "big_map"
      | C_list         -> "list"
      | C_set          -> "set"
      | C_unit         -> "unit"
      | C_string       -> "string"
      | C_nat          -> "nat"
      | C_mutez        -> "mutez"
      | C_timestamp    -> "timestamp"
      | C_int          -> "int"
      | C_address      -> "address"
      | C_bytes        -> "bytes"
      | C_key_hash     -> "key_hash"
      | C_key          -> "key"
      | C_signature    -> "signature"
      | C_operation    -> "operation"
      | C_contract     -> "contract"
      | C_chain_id     -> "chain_id"
    in
    fprintf ppf "CTOR %a %s()" Var.pp tv ct
  |SC_Alias       { a; b } -> fprintf ppf "Alias %a %a" Var.pp a Var.pp b
  |SC_Poly        _ -> fprintf ppf "Poly"
  |SC_Typeclass   _ -> fprintf ppf "TC"

let type_constraint : _ -> type_constraint_simpl -> unit = fun ppf { reason_simpl ; c_simpl } ->
  fprintf ppf "%a (reason: %s)" type_constraint_ c_simpl reason_simpl

let all_constraints ppf ac =
  fprintf ppf "[%a]" (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ";\n") type_constraint) ac

let aliases ppf (al : unionfind) =
  fprintf ppf "ALIASES %a" UF.print al

let structured_dbs : _ -> structured_dbs -> unit = fun ppf structured_dbs ->
  let { all_constraints = a ; aliases = b ; _ } = structured_dbs in
  fprintf ppf "STRUCTURED_DBS\n %a\n %a" all_constraints a aliases b

let already_selected : _ -> already_selected -> unit = fun ppf already_selected ->
  let _ = already_selected in
  fprintf ppf "ALREADY_SELECTED"

let state : _ -> typer_state -> unit = fun ppf state ->
  let { structured_dbs=a ; already_selected=b } = state in
  fprintf ppf "STATE %a %a" structured_dbs a already_selected b