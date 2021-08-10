[@@@warning "-42"]

(* Dependencies *)

module Region = Simple_utils.Region
module CST    = Cst.Jsligo

open Region
open Errors
open Trace

(* Useful modules *)

module SSet = Set.Make (String)

module Ord =
  struct
    type t = CST.variable
    let compare v1 v2 =
      String.compare v1.value v2.value
  end

module VarSet = Set.Make (Ord)

(* Checking the definition of reserved names (shadowing) *)

let reserved =
  let open SSet in
  empty
  |> add "await"
  |> add "break"
  |> add "case"
  |> add "catch"
  |> add "class"
  |> add "const"
  |> add "continue"
  |> add "debugger"
  |> add "default"
  |> add "delete"
  |> add "do"
  |> add "else"
  |> add "enum"
  |> add "export"
  |> add "extends"
  |> add "false"
  |> add "finally"
  |> add "for"
  |> add "function"
  |> add "if"
  |> add "import"
  |> add "in"
  |> add "instanceOf"
  |> add "new"
  |> add "null"
  |> add "return"
  |> add "super"
  |> add "switch"
  |> add "this"
  |> add "throw"
  |> add "true"
  |> add "try"
  |> add "typeof"
  |> add "var"
  |> add "void"
  |> add "while"
  |> add "with"
  |> add "yield"

  |> add "implements"
  |> add "interface"
  |> add "package"
  |> add "private"
  |> add "protected"
  |> add "public"

  |> add "arguments"
  |> add "eval"

let check_reserved_names ~raise vars =
  let is_reserved elt = SSet.mem elt.value reserved in
  let inter = VarSet.filter is_reserved vars in
  if not (VarSet.is_empty inter) then
    let clash = VarSet.choose inter in
    raise.raise @@ reserved_name clash
  else vars

let check_reserved_name ~raise var =
  if SSet.mem var.value reserved then
    raise.raise @@ reserved_name var
  else ()

(* Checking the linearity of patterns *)

open! CST

let rec vars_of_pattern ~raise env = function
  PVar var ->
    if VarSet.mem var env then
      raise.raise @@ non_linear_pattern var
    else VarSet.add var env 
| PConstr   p ->
    if VarSet.mem p env then
      raise.raise @@ non_linear_pattern p
    else VarSet.add p env 
| PDestruct {value = {property; target = {value = {binders; _}; _}; _}; _} -> 
    if VarSet.mem property env then
      raise.raise @@ non_linear_pattern property
    else (
      let env = vars_of_pattern ~raise env binders in
      VarSet.add property env
    )
| PObject   {value = {inside; _}; _}
| PArray    {value = {inside; _}; _} -> 
    let env = Utils.nsepseq_to_list inside |> check_patterns ~raise in
    env
| PAssign {value = {property; _}; _} -> 
    if VarSet.mem property env then
      raise.raise@@ non_linear_pattern property
    else VarSet.add property env 
| PWild _
| PRest _ ->
    env 

and check_linearity p = vars_of_pattern VarSet.empty p

(* Checking patterns *)

and check_pattern ~raise p =
  check_linearity ~raise p |> check_reserved_names ~raise

and check_patterns ~raise patterns =
  let add _acc p =
    let env = check_pattern ~raise p in
    env
  in List.fold ~f:add ~init:VarSet.empty patterns


(* Checking variants for duplicates *)

let check_variants ~raise variants =
  let rec add acc = function
    TString value
  | TVar value -> 
      if VarSet.mem value acc then
        raise.raise @@ duplicate_variant value
      else VarSet.add value acc
  | TProd {inside = {value = {inside; _}; _}; _ } as t -> (
    let items = Utils.nsepseq_to_list inside in
    match items with 
      hd :: [] -> add acc hd
    | TString _ as hd :: _ -> add acc hd
    | _ -> 
      raise.raise @@ not_supported_variant t
      )
  | _ as t -> 
    raise.raise @@ not_supported_variant t
  in
  let variants =
    List.fold ~f:add ~init:VarSet.empty variants
  in ignore variants

(* Checking object fields *)

let check_fields ~raise fields =
  let add acc ({value; _}: field_decl reg) =
    let field_name = (value: field_decl).field_name in
    if VarSet.mem field_name acc then
      raise.raise @@ duplicate_field_name value.field_name
    else
      VarSet.add value.field_name acc
  in ignore (List.fold ~f:add ~init:VarSet.empty fields)

let peephole_type ~raise : unit -> type_expr -> unit = fun _ t ->
  match t with
    TSum {value; _} ->
      let () = Utils.nsepseq_to_list value.variants |> check_variants ~raise in
    ()
  | TObject {value; _} ->
      let () = Utils.nsepseq_to_list value.ne_elements |> check_fields ~raise in
      ()
  | TProd _
  | TApp _
  | TFun _
  | TPar _
  | TString _
  | TVar _
  | TModA _
  | TInt _
  | TWild _ -> ()

let peephole_expression : unit -> expr -> unit = fun () _ ->
  ()

let check_binding ~raise ({value = {binders; _}; _}: CST.let_binding Region.reg) = 
  ignore (check_pattern ~raise binders)

let check_bindings ~raise bindings =
  let add _acc b =
    let () = check_binding ~raise b in
    ()
  in List.fold ~f:add ~init:() bindings

let rec peephole_statement ~raise : unit -> statement -> unit = fun _ s ->
  match s with
    SExpr e -> 
    let () = peephole_expression () e in
    ()
  | SNamespace {value = (_, name, _); _} ->
    let () = check_reserved_name ~raise name in 
    ()
  | SExport {value = (_, e); _} -> 
    peephole_statement ~raise () e
  | SLet   {value = {bindings; _}; _}
  | SConst {value = {bindings; _}; _} ->
    let () = Utils.nsepseq_to_list bindings |> check_bindings ~raise in 
    ()
  | SType  {value = {name; _}; _} ->
    let () = check_reserved_name ~raise name in 
    ()
  | SWhile {value = {expr; statement; _}; _}
  | SForOf {value = {expr; statement; _}; _} ->
    let () = peephole_expression () expr in
    let () = peephole_statement ~raise () statement in
    ()
  | SBlock  _
  | SCond   _
  | SReturn _
  | SBreak _
  | SImport _
  | SSwitch _ -> ()

let peephole ~raise : (unit,'err) Helpers.folder = {
  t = peephole_type ~raise;
  e = peephole_expression;
  d = peephole_statement ~raise;
}
