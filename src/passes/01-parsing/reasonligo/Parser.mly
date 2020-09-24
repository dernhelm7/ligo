%{
(* START HEADER *)

[@@@warning "-42"]

module Region = Simple_utils.Region
open Region
module CST = Cst.Reasonligo
open! CST

(*
  Convert a nsepseq to a chain of TFun's.

  Necessary to handle cases like:
  [type foo = (int, int) => int;]
*)

let wild_error e =
  match e with
    | EVar { value = "_"; _} as e ->
      let open! SyntaxError in
      raise (Error (InvalidWild e))
    | _ -> ()

(* END HEADER *)
%}

(* See [ParToken.mly] for the definition of tokens. *)

(* Entry points *)

%start contract interactive_expr
%type <Cst.Reasonligo.t> contract
%type <Cst.Reasonligo.expr> interactive_expr

(* Solves a shift/reduce problem that happens with records and
   sequences. To elaborate: [sequence_or_record_in]
   can be reduced to [expr -> Ident], but also to
   [field_assignment -> Ident].
*)

%nonassoc Ident
%nonassoc COLON

%%

(* RULES *)

(* The rule [sep_or_term(item,sep)] ("separated or terminated list")
   parses a non-empty list of items separated by [sep], and optionally
   terminated by [sep]. *)

sep_or_term_list(item,sep):
  nsepseq(item,sep) {
    $1, None
  }
| nseq(item sep {$1,$2}) {
    let (first,sep), tail = $1 in
    let rec trans (seq, prev_sep as acc) = function
      [] -> acc
    | (item,next_sep)::others ->
        trans ((prev_sep,item)::seq, next_sep) others in
    let list, term = trans ([],sep) tail
    in (first, List.rev list), Some term }

(* Compound constructs *)

par(X):
  "(" X ")" {
    let region = cover $1 $3
    and value  = ({lpar=$1; inside=$2; rpar=$3} : _ par)
    in {region; value} }

(* Sequences

   Series of instances of the same syntactical category have often to
   be parsed, like lists of expressions, patterns etc. The simplest of
   all is the possibly empty sequence (series), parsed below by
   [seq]. The non-empty sequence is parsed by [nseq]. Note that the
   latter returns a pair made of the first parsed item (the parameter
   [X]) and the rest of the sequence (possibly empty). This way, the
   OCaml typechecker can keep track of this information along the
   static control-flow graph. The rule [sepseq] parses possibly empty
   sequences of items separated by some token (e.g., a comma), and
   rule [nsepseq] is for non-empty such sequences. See module [Utils]
   for the types corresponding to the semantic actions of those
   rules.
*)

(* Possibly empty sequence of items *)

seq(item):
  (**)           {     [] }
| item seq(item) { $1::$2 }

(* Non-empty sequence of items *)

nseq(item):
  item seq(item) { $1,$2 }

(* Non-empty separated sequence of items *)

nsepseq(item,sep):
  item                       {                        $1, [] }
| item sep nsepseq(item,sep) { let h,t = $3 in $1, ($2,h)::t }

(* Helpers *)

%inline type_name   : "<ident>"  { $1 }
%inline field_name  : "<ident>"  { $1 }
%inline struct_name : "<ident>"  { $1 }
%inline module_name : "<constr>" { $1 }

(* Non-empty comma-separated values (at least two values) *)

tuple(item):
  item "," nsepseq(item,",") { let h,t = $3 in $1, ($2,h)::t }

(* Possibly empty semicolon-separated values between brackets *)

list__(item):
  "[" sep_or_term_list(item,";")? "]" {
    let compound = Some (Brackets ($1,$3))
    and region = cover $1 $3 in
    let elements, terminator =
      match $2 with
        None -> None, None
      | Some (elements, terminator) ->
          Some elements, terminator in
    let value = {compound; elements; terminator}
    in {region; value} }

(* Main *)

contract:
  declarations EOF { {decl=$1; eof=$2} }

declarations:
  declaration              { $1,[] : CST.declaration Utils.nseq }
| declaration declarations { Utils.nseq_cons $1 $2              }

declaration:
| type_decl ";"?           { TypeDecl  $1 }
| let_declaration ";"?     { ConstDecl $1 }

(* Type declarations *)

type_decl:
  "type" type_name "=" type_expr {
    Scoping.check_reserved_name $2;
    let region = cover $1 (type_expr_to_region $4)
    and value = {kwd_type   = $1;
                 name       = $2;
                 eq         = $3;
                 type_expr  = $4}
    in {region; value} }

type_expr:
  fun_type | sum_type | record_type { $1 }

fun_type:
  cartesian { $1 }
| cartesian "=>" fun_type {
    let start  = type_expr_to_region $1
    and stop   = type_expr_to_region $3 in
    let region = cover start stop in
    TFun {region; value=$1,$2,$3} }

cartesian:
  core_type { $1 }
| par(tuple (core_type)) {
    TProd $1
  }

type_args:
  tuple(fun_type) {     $1 }
| fun_type        { $1, [] }

core_type:
  type_name      {    TVar $1 }
| "_"            {   TWild $1 }
| par(type_expr) {    TPar $1 }
| "<string>"     { TString $1 }
| module_name "." type_name {
    let module_name = $1.value in
    let type_name   = $3.value in
    let value       = module_name ^ "." ^ type_name in
    let region      = cover $1.region $3.region
    in TVar {region; value}
  }
| type_name par(type_args) {
   let region = cover $1.region $2.region
   in TApp {region; value = $1,$2} }

sum_type:
  ioption("|") nsepseq(variant,"|") {
    Scoping.check_variants (Utils.nsepseq_to_list $2);
    let region = nsepseq_to_region (fun x -> x.region) $2
    in TSum {region; value=$2} }

variant:
  "<constr>" { {$1 with value={constr=$1; arg=None}} }
| "<constr>" "(" fun_type ")" {
    let region = cover $1.region $4
    and value  = {constr=$1; arg = Some (ghost,$3)}
    in {region; value} }

record_type:
  "{" sep_or_term_list(field_decl,",") "}" {
    let ne_elements, terminator = $2 in
    let () = Utils.nsepseq_to_list ne_elements
             |> Scoping.check_fields in
    let region = cover $1 $3
    and value  = {compound = Some(Braces ($1,$3)); ne_elements; terminator}
    in TRecord {region; value} }

type_expr_field:
  core_type | sum_type | record_type { $1 }

field_decl:
  field_name {
    let value = {field_name=$1; colon=ghost; field_type = TVar $1}
    in {$1 with value}
  }
| field_name ":" type_expr_field {
    let stop   = type_expr_to_region $3 in
    let region = cover $1.region stop
    and value  = {field_name=$1; colon=$2; field_type=$3}
    in {region; value} }

(* Top-level definitions *)

let_declaration:
  seq(Attr) "let" ioption("rec") let_binding {
    let attributes = $1 in
    let kwd_let    = $2 in
    let kwd_rec    = $3 in
    let binding    = $4 in
    let value      = kwd_let, kwd_rec, binding, attributes in
    let start      = match $1 with
                       [] -> $2
                     | l  -> last (fun x -> x.region) l
    and stop       = expr_to_region binding.let_rhs in
    let region     = cover start stop
    in {region; value} }

let_binding:
  let_pattern_simple type_annotation? "=" expr {
    wild_error $4;
    {binders = $1; lhs_type=$2; eq=$3; let_rhs=$4}
  }

let_pattern_simple :
  Ident                       {                Scoping.check_reserved_name $1; PVar $1 }
| "_"                         {                                               PWild $1 }
| unit                        {                                               PUnit $1 }
| record_pattern              {         Scoping.check_pattern (PRecord $1); PRecord $1 }
| par (closed_irrefutable)    { Scoping.check_pattern $1.value.inside; $1.value.inside }
| tuple (sub_irrefutable)     {
    Utils.nsepseq_iter Scoping.check_pattern $1;
    let region  = nsepseq_to_region pattern_to_region $1 in
    PTuple {value=$1; region}
}

type_annotation:
  ":" type_expr { $1,$2 }

(* Patterns *)

irrefutable:
  sub_irrefutable { $1 }
| tuple(sub_irrefutable) {
    let hd, tl = $1 in
    let start  = pattern_to_region hd in
    let stop   = last fst tl in
    let region = cover start stop in
    PTuple {region; value=$1} }

sub_irrefutable:
  "<ident>"                                              {    PVar $1 }
| "_"                                                    {   PWild $1 }
| unit                                                   {   PUnit $1 }
| record_pattern                                         { PRecord $1 }
| par(closed_irrefutable)                                {    PPar $1 }

closed_irrefutable:
  irrefutable                                            {         $1 }
| constr_pattern                                         { PConstr $1 }
| typed_pattern                                          {  PTyped $1 }

typed_pattern:
  irrefutable ":" type_expr  {
    let start  = pattern_to_region $1 in
    let stop   = type_expr_to_region $3 in
    let region = cover start stop in
    let value  = {pattern=$1; colon=$2; type_expr=$3}
    in {region; value} }

pattern:
  core_pattern { $1 }
| "[" sub_pattern "," "..." sub_pattern "]" {
    let start  = pattern_to_region $2 in
    let stop   = pattern_to_region $5 in
    let region = cover start stop in
    let value  =
      { lbracket = $1;
	lpattern = $2;
	comma    = $3;
	ellipsis = $4;
	rpattern = $5;
	rbracket = $6
      }
    in PList (PCons {value;region})
  }
| tuple(sub_pattern) {
    let hd, tl = $1 in
    let start  = pattern_to_region hd in
    let stop   = last fst tl in
    let region = cover start stop
    in PTuple {value=$1; region} }

sub_pattern:
  par(sub_pattern)                                       {    PPar $1 }
| core_pattern                                           {         $1 }

core_pattern:
  "<ident>"                                            {      PVar $1 }
| "_"                                                  {     PWild $1 }
| unit                                                 {     PUnit $1 }
| "<int>"                                              {      PInt $1 }
| "<nat>"                                              {      PNat $1 }
| "<bytes>"                                            {    PBytes $1 }
| "<string>"                                           {   PString $1 }
| "<verbatim>"                                         { PVerbatim $1 }
| par(ptuple)                                          {      PPar $1 }
| list__(sub_pattern)                          { PList (PListComp $1) }
| constr_pattern                                       {   PConstr $1 }
| record_pattern                                       {   PRecord $1 }

record_pattern:
  "{" sep_or_term_list(field_pattern,",") "}" {
    let ne_elements, terminator = $2 in
    let region = cover $1 $3 in
    let value  = {compound = Some (Braces ($1,$3));
                  ne_elements;
                  terminator}
    in {region; value} }

field_pattern:
  field_name "=" sub_pattern {
    let start  = $1.region in
    let stop   = pattern_to_region $3 in
    let region = cover start stop in
    let value  = {field_name=$1; eq=$2; pattern=$3}
    in {region; value} }

constr_pattern:
  "None" { PNone $1 }
| "Some" sub_pattern {
    let stop   = pattern_to_region $2 in
    let region = cover $1 stop
    and value  = $1, $2 in
    PSomeApp {region; value}
  }
| "true"  {  PTrue $1 }
| "false" { PFalse $1 }
| "<constr>" sub_pattern {
    let region = cover $1.region (pattern_to_region $2)
    in PConstrApp {region; value = $1, Some $2}
  }
| "<constr>" { PConstrApp {$1 with value=$1,None} }

ptuple:
  tuple(sub_pattern) {
    let hd, tl = $1 in
    let start  = pattern_to_region hd in
    let stop   = last fst tl in
    let region = cover start stop
    in PTuple {value=$1; region} }

unit:
  "(" ")" { {region = cover $1 $2; value = $1, $2} }

(* Expressions *)

interactive_expr:
  expr_with_let_expr EOF { $1 }

expr:
  base_cond | switch_expr(base_cond) { $1 }

base_cond:
  base_expr | conditional(expr_with_let_expr) {
    wild_error $1;
    $1 }

type_expr_simple_args:
  par(nsepseq(type_expr_simple, ",")) { $1 }

type_expr_simple:
  type_name type_expr_simple_args? {
    let args = $2 in
    match args with
      Some {value; _} ->
        let region = cover $1.region value.rpar in
        let value  = $1, {region; value}
        in TApp {region; value}
    | None -> TVar $1
  }
| par(type_expr_simple){ TPar $1}
| par(tuple(type_expr_simple)) {
    TProd $1
  }
| "(" type_expr_simple "=>" type_expr_simple ")" {
    TPar {
      value = {
        lpar = $1;
        rpar = $5;
        inside = TFun {
          region = cover (type_expr_to_region $2) (type_expr_to_region $4);
          value=$2,$3,$4
        }
      };
      region = cover $1 $5;
    }
}

type_annotation_simple:
  ":" type_expr_simple { $1,$2 }


fun_expr(right_expr):
  disj_expr_level "=>" right_expr {
    let arrow, body = $2, $3 in
    let start       = expr_to_region $1
    and stop        = expr_to_region body in
    let region      = cover start stop in

    let rec arg_to_pattern = function
      EVar v ->
        if v.value = "_" then
          PWild v.region
        else (
          Scoping.check_reserved_name v;
          PVar v
        )
    | EAnnot {region; value = {inside = EVar v, colon, typ; _}} ->
        Scoping.check_reserved_name v;
        let value = {pattern = PVar v; colon; type_expr = typ} in
        PTyped {region; value}
    | EPar p ->
        let value =
          {p.value with inside = arg_to_pattern p.value.inside}
        in PPar {p with value}
    | EUnit u -> PUnit u
    | ETuple { value; region } ->
        PTuple { value = Utils.nsepseq_map arg_to_pattern value; region}
    | EAnnot {region; value = {inside = t, colon, typ; _}} ->
        let value = { pattern = arg_to_pattern t; colon; type_expr = typ} in
        PTyped {region; value}
    | e ->
        let open! SyntaxError in
        raise (Error (WrongFunctionArguments e)) in
    let fun_args_to_pattern = function
      EAnnot {
        value = {
          inside = ETuple _ as e, _, _;
          _};
        _} ->
        (*  ((foo:x, bar) : type)  *)
        arg_to_pattern e
      | EAnnot {
          value = {
            inside = EPar _ as e, _, _;
            _};
          _} ->
          (* ((foo:x, bar) : type) *)
         arg_to_pattern e
      (*function as argument *)
      | EPar {value = {inside = EFun {
          value = {
              binders = PTyped { value = { pattern; colon; type_expr }; region = fun_region };
              arrow;
              body;
              _
          };
          _
        }; lpar;rpar; }; region} ->
        let expr_to_type = function
        | EVar v -> TVar v
        | e -> let open! SyntaxError
            in raise (Error (WrongFunctionArguments e))
        in
        let type_expr = (
            TFun {
              value = type_expr, arrow, expr_to_type body;
              region = fun_region
            }
        )
        in
        PPar {value = {inside =
          PTyped {
            value = {
              pattern;
              colon;
              type_expr
            };
            region;
          };
          lpar;
          rpar;
          };
          region;
        }
      | EPar _ as e ->
          arg_to_pattern e
      | EAnnot _ as e ->
          arg_to_pattern e
      | ETuple _ as e ->
          arg_to_pattern e
      | EUnit _ as e ->
          arg_to_pattern e
      | EVar _ as e ->
          arg_to_pattern e
      | e ->
      let open! SyntaxError
            in raise (Error (WrongFunctionArguments e))
    in
    let binders = fun_args_to_pattern $1 in
    let lhs_type = match $1 with
      EAnnot {value = {inside = _ , _, t; _}; region = r} -> Some (r,t)
      | _ -> None
    in
    let value = {binders;
             lhs_type;
             arrow;
             body
            }
    in EFun {region; value} }

base_expr:
  disj_expr_level | fun_expr(expr) { $1 }

conditional(right_expr):
  if_then_else(right_expr) | if_then(right_expr) { $1 }

parenthesized_expr:
  "{" expr "}" | "(" expr ")" { $2 }

if_then(right_expr):
  "if" parenthesized_expr "{" closed_if ";"? "}" {
    let region   = cover $1 $6 in
    let value    = {kwd_if   = $1;
                    test     = $2;
                    ifso     = {lbrace=$3; inside=($4,$5); rbrace=$6};
                    ifnot    = None}
    in ECond {region; value} }

if_then_else(right_expr):
  "if" parenthesized_expr "{" closed_if ";"?  "}"
  "else" "{" right_expr ";"? "}" {
    let region = cover $1 $11 in
    let value  = {kwd_if   = $1;
                  test     = $2;
                  ifso     = {lbrace=$3; inside=($4,$5); rbrace=$6};
                  ifnot    = Some ($7,{lbrace=$8; inside=($9,$10); rbrace=$11})}
    in ECond {region; value} }

base_if_then_else__open(x):
  base_expr | if_then_else(x) { $1 }

base_if_then_else:
  base_if_then_else__open(base_if_then_else) { $1 }

closed_if:
  base_if_then_else__open(closed_if)
| switch_expr(base_if_then_else) { $1 }
| let_expr(expr_with_let_expr) { $1 }

switch_expr(right_expr):
  "switch" switch_expr_ "{" cases(right_expr) "}" {
    let start = $1
    and stop = $5 in
    let region = cover start stop
    and cases = $4 in
    let value = {
      kwd_switch = $1;
      expr      = $2;
      lbrace    = $3;
      cases;
      rbrace  = $5 }
    in ECase {region; value} }

switch_expr_:
  par(expr)   { $1.value.inside }
| core_expr_2 {              $1 }

cases(right_expr):
  nseq(case_clause(right_expr)) {
    let hd, tl = $1 in
    let nseq = snd hd, tl in
    {
      region = nsepseq_to_region (fun x -> x.region) nseq;
      value  = nseq }
  }

case_clause(right_expr):
  "|" pattern "=>" right_expr ";"? {
    Scoping.check_pattern $2;
    let start  = pattern_to_region $2
    and stop   = expr_to_region $4 in
    let region = cover start stop
    and value  = {pattern=$2; arrow=$3; rhs=$4; terminator=$5 }
    in $1,{region; value} }

let_expr(right_expr):
  seq(Attr) "let" ioption("rec") let_binding ";" right_expr {
    let attributes = $1 in
    let kwd_let = $2 in
    let kwd_rec = $3 in
    let binding = $4 in
    let semi    = $5 in
    let body    = $6 in
    let stop    = expr_to_region $6 in
    let region  = cover $2 stop
    and value   = {kwd_let; kwd_rec; binding; semi; body; attributes}
    in ELetIn {region; value} }

disj_expr_level:
  disj_expr
| conj_expr_level { $1 }
| par(tuple(disj_expr_level)) type_annotation_simple? {
    let region = nsepseq_to_region expr_to_region $1.value.inside in
    let tuple  = ETuple {value=$1.value.inside; region} in
    let par =
      EPar {$1 with value = {$1.value with inside = tuple}} in
    match $2 with
      Some (colon, typ) ->
        let region = cover $1.region (type_expr_to_region typ)
        and value = {$1.value with inside = par,colon,typ}
        in EAnnot {region; value}
    | None -> par
    }

bin_op(arg1,op,arg2):
  arg1 op arg2 {
    let start  = expr_to_region $1 in
    let stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = { arg1=$1; op=$2; arg2=$3}
    in {region; value} }

disj_expr:
  bin_op(disj_expr_level, "||", conj_expr_level)
| bin_op(disj_expr_level, "or", conj_expr_level) {
    ELogic (BoolExpr (Or $1)) }

conj_expr_level:
  comp_expr_level { $1 }
| bin_op(conj_expr_level, "&&", comp_expr_level) {
    ELogic (BoolExpr (And $1)) }

comp_expr_level:
  bin_op(comp_expr_level, "<", cat_expr_level) {
    ELogic (CompExpr (Lt $1)) }
| bin_op(comp_expr_level, "<=", cat_expr_level) {
    ELogic (CompExpr (Leq $1)) }
| bin_op(comp_expr_level, ">", cat_expr_level) {
    ELogic (CompExpr (Gt $1)) }
| bin_op(comp_expr_level, ">=", cat_expr_level) {
    ELogic (CompExpr (Geq $1)) }
| bin_op(comp_expr_level, "==", cat_expr_level) {
    ELogic (CompExpr (Equal $1)) }
| bin_op(comp_expr_level, "!=", cat_expr_level) {
    ELogic (CompExpr (Neq $1)) }
| cat_expr_level { $1 }

cat_expr_level:
  bin_op(add_expr_level, "++", cat_expr_level)    {  EString (Cat $1) }
| add_expr_level                                  {                $1 }

add_expr_level:
  bin_op(add_expr_level, "+", mult_expr_level)      { EArith (Add $1) }
| bin_op(add_expr_level, "-", mult_expr_level)      { EArith (Sub $1) }
| mult_expr_level                                   {              $1 }

mult_expr_level:
  bin_op(mult_expr_level, "*", unary_expr_level)   { EArith (Mult $1) }
| bin_op(mult_expr_level, "/", unary_expr_level)   {  EArith (Div $1) }
| bin_op(mult_expr_level, "mod", unary_expr_level) {  EArith (Mod $1) }
| unary_expr_level                                 {               $1 }

unary_expr_level:
  call_expr_level { $1 }
| "-" call_expr_level {
    let start  = $1 in
    let stop   = expr_to_region $2 in
    let region = cover start stop
    and value  = {op=$1; arg=$2}
    in EArith (Neg {region; value})
  }
| "!" call_expr_level {
    let start  = $1 in
    let stop   = expr_to_region $2 in
    let region = cover start stop
    and value  = {op=$1; arg=$2} in
    ELogic (BoolExpr (Not {region; value})) }

call_expr_level:
  call_expr_level_in type_annotation_simple? {
    let region =
      match $2 with
        Some (_, s) ->
          cover (expr_to_region $1) (type_expr_to_region s)
      | None -> expr_to_region $1 in
    match $2 with
      Some (colon, t) ->
        let value = {lpar=ghost; inside=$1,colon,t; rpar=ghost}
        in EAnnot {region; value}
    | None -> $1 }

call_expr_level_in:
  call_expr | constr_expr | core_expr { $1 }

constr_expr:
  "None" {
     EConstr (ENone $1)
  }
| "Some" core_expr {
    let region = cover $1 (expr_to_region $2)
    in EConstr (ESomeApp {value=$1,$2; region})
  }
| "<constr>" core_expr {
    let region = cover $1.region (expr_to_region $2) in
    EConstr (EConstrApp {region; value=$1, Some $2})
   }
| "<constr>" {
    EConstr (EConstrApp {$1 with value=$1, None}) }

call_expr:
  core_expr par(nsepseq(expr, ",")) {
    let start  = expr_to_region $1 in
    let stop   = $2 in
    let region = cover start stop.region in
    ECall {region; value = $1,Multiple $2}
  }
| core_expr unit {
    let start  = expr_to_region $1 in
    let stop   = $2.region in
    let region = cover start stop
    and value  = $1, Unit $2
    in ECall {region; value} }

common_expr:
  "<int>"                             {                 EArith (Int $1) }
| "<mutez>"                           {               EArith (Mutez $1) }
| "<nat>"                             {                 EArith (Nat $1) }
| "<bytes>"                           {                       EBytes $1 }
| "<ident>" | module_field            {                         EVar $1 }
| projection                          {                        EProj $1 }
| "_"                                 { EVar {value = "_"; region = $1} }
| update_record                       {                      EUpdate $1 }
| "<string>"                          {             EString (String $1) }
| "<verbatim>"                        {           EString (Verbatim $1) }
| unit                                {                        EUnit $1 }
| "false"                             {    ELogic (BoolExpr (False $1)) }
| "true"                              {     ELogic (BoolExpr (True $1)) }
| code_inj                            {                     ECodeInj $1 }

core_expr_2:
  common_expr   {                   $1 }
| list__(expr)  { EList (EListComp $1) }

list_or_spread:
  "[" expr "," sep_or_term_list(expr, ",") "]" {
    let elts, terminator = $4 in
    let elts = Utils.nsepseq_cons $2 $3 elts in
    let value = {
      compound = Some (Brackets ($1,$5));
      elements = Some elts;
      terminator}
    and region = cover $1 $5 in
    EList (EListComp {region; value})
  }
| "[" expr "," "..." expr "]" {
    let region = cover $1 $6
    and value : cons_expr = {lbracket=$1; lexpr=$2; comma=$3; ellipsis=$4; rexpr=$5; rbracket=$6}
    in EList (ECons {region; value})
  }
| "[" expr? "]" {
    let compound = Some (Brackets ($1,$3))
    and elements =
      match $2 with
        None -> None
      | Some element -> Some (element, []) in
    let value = {compound; elements; terminator=None}
    and region = cover $1 $3 in
    EList (EListComp {region; value}) }

core_expr:
  common_expr
| list_or_spread      {         $1 }
| sequence            {    ESeq $1 }
| record              { ERecord $1 }
| par(expr)           {    EPar $1 }

module_field:
  module_name "." module_fun {
    let region = cover $1.region $3.region in
    {region; value = $1.value ^ "." ^ $3.value} }

module_fun:
  field_name { $1 }
| "or"       { {value="or";  region=$1} }

selection:
  "[" "<int>" "]" selection {
    let r, (hd, tl) = $4 in
    let result: (selection, dot) Utils.nsepseq =
      Component $2, (ghost, hd) :: tl
    in r, result
  }
| "." field_name selection {
    let r, (hd, tl) = $3 in
    let result: (selection, dot) Utils.nsepseq =
      FieldName $2, ($1, hd) :: tl
    in r, result
  }
| "." field_name  {    $1, (FieldName $2, []) }
| "[" "<int>" "]" { ghost, (Component $2, []) }

projection:
  struct_name selection {
    let start  = $1.region in
    let stop   = nsepseq_to_region selection_to_region (snd $2) in
    let region = cover start stop
    and value  = {struct_name = $1;
                  selector    = fst $2;
                  field_path  = snd $2}
    in {region; value}
  }
| module_name "." field_name selection {
    let module_name = $1 in
    let field_name  = $3 in
    let value       = module_name.value ^ "." ^ field_name.value in
    let struct_name = {$1 with value} in
    let start       = $1.region in
    let stop        = nsepseq_to_region selection_to_region (snd $4) in
    let region      = cover start stop
    and value       = {struct_name;
                       selector   = fst $4;
                       field_path = snd $4}
    in {region; value} }

path:
 "<ident>"   { Name $1 }
| projection { Path $1 }

update_record:
  "{" "..." path "," sep_or_term_list(field_path_assignment,",") "}" {
    let region = cover $1 $6 in
    let ne_elements, terminator = $5 in
    let value = {
      lbrace   = $1;
      ellipsis = $2;
      record   = $3;
      comma    = $4;
      updates  = {value = {compound = None;
                          ne_elements;
                          terminator};
                 region = cover $4 $6};
      rbrace   = $6}
    in {region; value} }

code_inj:
  "<lang>" expr "]" {
    let region   = cover $1.region $3
    and value    = {language=$1; code=$2; rbracket=$3}
    in {region; value} }

expr_with_let_expr:
  expr
| let_expr(expr_with_let_expr) { $1 }

exprs:
  expr_with_let_expr ";"? {
    (($1, []), $2)
  }
| expr_with_let_expr ";" exprs {
  let rec fix_let_in a b c =
    match a with
    | ELetIn {value = {body; _} as v; _} -> (
      let end_ = (nsepseq_to_region expr_to_region (fst c)) in
      let sequence_region =
        cover (expr_to_region body) end_
      in
      let val_ =
        match body with
        | ELetIn _ -> fst (fix_let_in body b c)
        | e -> Utils.nsepseq_cons e b (fst c)
      in
      let sequence = ESeq {
        value = {
          compound   = None;
          elements   = Some val_;
          terminator = snd c};
        region = sequence_region
      }
      in
      let region =
        cover (expr_to_region a) end_
      in
      let let_in =
        ELetIn {
          value = {
            v with
            body = sequence
          };
          region
        }
      in
      ((let_in, []), snd c)
    )
    | e -> Utils.nsepseq_cons e b (fst c), None
  in
  fix_let_in $1 $2 $3
}

more_field_assignments:
  "," sep_or_term_list(field_assignment_punning,",") {
    let elts, _region = $2
    in $1, elts }

sequence:
  "{" exprs "}" {
    let elts, _region = $2 in
    let compound = Some (Braces ($1, $3)) in
    let value    = {compound;
                    elements = Some elts;
                    terminator = None} in
    let region   = cover $1 $3
    in {region; value} }

record:
  "{" field_assignment more_field_assignments? "}" {
    let compound = Some (Braces ($1,$4)) in
    let region   = cover $1 $4 in

    match $3 with
    | Some (comma, elts) ->
        let ne_elements = Utils.nsepseq_cons $2 comma elts in
        { value = {compound; ne_elements; terminator = None}; region }
    | None ->
        let ne_elements = ($2,[]) in
        { value = {compound; ne_elements; terminator = None}; region }
  }
| "{" field_name more_field_assignments "}" {
    let value = {
      field_name = $2;
      assignment = ghost;
      field_expr = EVar $2 } in
    let field_name = {$2 with value} in
    let comma, elts = $3 in
    let ne_elements = Utils.nsepseq_cons field_name comma elts in
    let compound = Some (Braces ($1,$4)) in
    let region   = cover $1 $4 in
    {value = {compound; ne_elements; terminator = None}; region} }

field_assignment_punning:
  (* This can only happen with multiple fields -
     one item punning does NOT work in ReasonML *)
  field_name {
    let value = {field_name = $1;
                 assignment = ghost;
                 field_expr = EVar $1}
    in {$1 with value}
  }
| field_assignment { $1 }

field_assignment:
  field_name ":" expr {
    let region = cover $1.region (expr_to_region $3)
    and value  = {field_name = $1;
                  assignment = $2;
                  field_expr = $3}
    in {region; value} }

field_path_assignment:
  path ":" expr {
    let region = cover (path_to_region $1) (expr_to_region $3)
    and value  = {field_path=$1; assignment=$2; field_expr=$3}
    in {region; value} }
