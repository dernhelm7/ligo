open Ast_typed
open Stage_common.Constant
module Protocols = Protocols
let star = ()

let basic_types : (type_variable * type_expression) list = [
    (v_bool , t_sum_ez [ ("True" ,t_unit ()); ("False",t_unit ()) ] ) ;
    (v_string , t_constant string_name []) ;
    (v_bytes , t_constant bytes_name []) ;
    (v_int , t_constant int_name []) ;
    (v_nat , t_constant nat_name []) ;
    (v_unit , t_constant unit_name []) ;
    (v_option , t_abstraction1 option_name star) ;
  ]

let michelson_base : (type_variable * type_expression) list = [
    (v_operation , t_constant operation_name []) ;
    (v_tez , t_constant tez_name []) ;
    (v_address , t_constant address_name []) ;
    (v_signature , t_constant signature_name []) ;
    (v_key , t_constant key_name []) ;
    (v_key_hash , t_constant key_hash_name []) ;
    (v_timestamp , t_constant timestamp_name []) ;
    (v_list , t_abstraction1 list_name star) ;
    (v_big_map , t_abstraction2 big_map_name star star);
    (v_map , t_abstraction2 map_name star star) ;
    (v_set , t_abstraction1 set_name star);
    (v_contract , t_abstraction1 contract_name star);
    (v_map_or_big_map , t_abstraction2 map_or_big_map_name star star);
    (v_michelson_or , t_abstraction2 michelson_or_name star star);
    (v_michelson_pair , t_abstraction2 michelson_pair_name star star);
    (v_chain_id , t_constant chain_id_name []) ;
    (v_baker_hash , t_constant baker_hash_name []);
    (v_pvss_key , t_constant pvss_key_name []);
    (v_sapling_state , t_abstraction1 sapling_state_name star);
    (v_sapling_trasaction , t_abstraction1 sapling_transaction_name star);
    (v_baker_operation , t_constant baker_operation_name []);
    (v_bls12_381_g1 , t_constant bls12_381_g1_name []);
    (v_bls12_381_g2 , t_constant bls12_381_g2_name []);
    (v_bls12_381_fr ,  t_constant bls12_381_fr_name []);
    (v_never , t_constant never_name []);
    (v_ticket , t_abstraction1 ticket_name star);
]

let edo_types = basic_types @ michelson_base

let wrap_var s = Location.wrap @@ Var.of_name s

let e_raw_code code typ =
  {
    expression_content = E_raw_code { language = "Michelson" ; code = {
      expression_content = E_literal (Literal_string (Simple_utils.Ligo_string.verbatim code)) ;
      location = Location.generated ;
      type_expression = typ ;     
    } ; } ;
    location = Location.generated ;
    type_expression = typ ;
  }

let add_bindings_in_env bs env =
  List.fold_left bs ~init:env ~f:(fun env (v,e) -> 
    Environment.add_ez_declaration (wrap_var v) e env)

let add_types_in_module_env ts env = 
  List.fold_left ts ~init:env ~f:(fun env (v,t) -> 
    Environment.add_type v t env)

let make_module type_env parent_env module_name bindings = 
  let module_env = add_bindings_in_env bindings Environment.empty in
  let module_env = add_types_in_module_env type_env module_env in
  Environment.add_module ~built_in:true module_name module_env parent_env 

let string_module t e = make_module t e "String" [
  ("length", e_raw_code "{ SIZE }" (t_function (t_string ()) (t_nat ()) ()) ) ;
  ("size"  , e_raw_code "{ SIZE }" (t_function (t_string ()) (t_nat ()) ())) ;
  ("slice" , e_raw_code "{ LAMBDA
  (pair nat nat)
  (lambda string string)
  { UNPAIR ;
    SWAP ;
    PAIR ;
    LAMBDA
      (pair (pair nat nat) string)
      string
      { UNPAIR ;
        UNPAIR ;
        DIG 2 ;
        SWAP ;
        DIG 2 ;
        SLICE ;
        IF_NONE { PUSH string \"SLICE\" ; FAILWITH } {} } ;
    SWAP ;
    APPLY } ;
SWAP ;
APPLY }" (t_function (t_nat ()) (t_function (t_nat ()) (t_function (t_string ()) (t_string ()) ()) ()) ()));
  ("sub"   , e_raw_code "{ LAMBDA
  (pair nat nat)
  (lambda string string)
  { UNPAIR ;
    SWAP ;
    PAIR ;
    LAMBDA
      (pair (pair nat nat) string)
      string
      { UNPAIR ;
        UNPAIR ;
        DIG 2 ;
        SWAP ;
        DIG 2 ;
        SLICE ;
        IF_NONE { PUSH string \"SLICE\" ; FAILWITH } {} } ;
    SWAP ;
    APPLY } ;
SWAP ;
APPLY }" (t_function (t_nat ()) (t_function (t_nat ()) (t_function (t_string ()) (t_string ()) ()) ()) ())) ;
  ("concat", e_raw_code "{ LAMBDA (pair string string) string { UNPAIR ; CONCAT } ; SWAP ; APPLY }" 
  (t_function (t_string ()) (t_function (t_string ()) (t_string ()) ()) ())) ; 
]

let meta_ligo_types : (type_variable * type_expression) list =
  edo_types @ [
    (v_test_michelson, t_constant test_michelson_name []) ;
    (v_test_exec_error, t_test_exec_error () ) ;
    (v_test_exec_result , t_test_exec_result () ) ;
    (v_account , t_constant account_name []) ;
    (v_typed_address , t_abstraction2 typed_address_name star star) ;
    (v_time , t_constant time_name []) ;
    (v_mutation, t_constant mutation_name []);
    (v_failure, t_constant failure_name []);
  ]

let default : Protocols.t -> environment = function
  | Protocols.Edo -> Environment.of_list_type edo_types |> string_module edo_types

let default_with_test : Protocols.t -> environment = function
  | Protocols.Edo -> Environment.of_list_type meta_ligo_types |> string_module meta_ligo_types
