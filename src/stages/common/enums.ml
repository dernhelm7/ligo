type z = Z.t
type ligo_string = Simple_utils.Ligo_string.t [@@deriving yojson]

let [@warning "-32"] z_to_yojson x = `String (Z.to_string x)
let [@warning "-32"] z_of_yojson x =
  try match x with
    | `String s -> Ok (Z.of_string s)
    | _ -> Utils.error_yojson_format "JSON string"
  with
  | Invalid_argument _ ->
    Error "Invalid formatting.
            The Zarith library does not know how to handle this formatting."

let bytes_to_yojson b = `String (Bytes.to_string b)

type layout =
  | L_comb
  | L_tree

type literal =
  | Literal_unit
  | Literal_int of z
  | Literal_nat of z
  | Literal_timestamp of z
  | Literal_mutez of z
  | Literal_string of ligo_string
  | Literal_bytes of bytes
  | Literal_address of string
  | Literal_signature of string
  | Literal_key of string
  | Literal_key_hash of string
  | Literal_chain_id of string
  | Literal_operation of bytes
[@@deriving yojson]

let literal_to_enum = function
  | Literal_unit        ->  1
  | Literal_int _       ->  2
  | Literal_nat _       ->  3
  | Literal_timestamp _ ->  4
  | Literal_mutez _     ->  5
  | Literal_string _    ->  6
  | Literal_bytes _     ->  7
  | Literal_address _   ->  8
  | Literal_signature _ ->  9
  | Literal_key _       -> 10
  | Literal_key_hash _  -> 11
  | Literal_chain_id _  -> 12
  | Literal_operation _ -> 13

type constant' =
  | C_INT [@printer PP_helpers.const "INT"]
  | C_UNIT [@printer PP_helpers.const "UNIT"]
  | C_NEVER [@printer PP_helpers.const "NEVER"]
  | C_NIL [@printer PP_helpers.const "NIL"]
  | C_NOW [@printer PP_helpers.const "NOW"]
  | C_IS_NAT [@printer PP_helpers.const "IS_NAT"]
  | C_SOME [@printer PP_helpers.const "SOME"]
  | C_NONE [@printer PP_helpers.const "NONE"]
  | C_ASSERTION [@printer PP_helpers.const "ASSERTION"]
  | C_ASSERT_SOME [@printer PP_helpers.const "ASSERT_SOME"]
  | C_ASSERT_INFERRED [@printer PP_helpers.const "ASSERT_INFERRED"]
  | C_FAILWITH [@printer PP_helpers.const "FAILWITH"]
  | C_UPDATE [@printer PP_helpers.const "UPDATE"]
  (* Loops *)
  | C_ITER [@printer PP_helpers.const "ITER"]
  | C_FOLD_WHILE [@printer PP_helpers.const "FOLD_WHILE"]
  | C_FOLD_CONTINUE [@printer PP_helpers.const "CONTINUE"]
  | C_FOLD_STOP [@printer PP_helpers.const "STOP"]
  | C_LOOP_LEFT [@printer PP_helpers.const "LOOP_LEFT"]
  | C_LOOP_CONTINUE [@printer PP_helpers.const "LOOP_CONTINUE"]
  | C_LOOP_STOP [@printer PP_helpers.const "LOOP_STOP"]
  | C_FOLD [@printer PP_helpers.const "FOLD"]
  | C_FOLD_LEFT [@printer PP_helpers.const "FOLD_LEFT"]
  | C_FOLD_RIGHT [@printer PP_helpers.const "FOLD_RIGHT"]
  (* MATH *)
  | C_NEG [@printer PP_helpers.const "NEG"]
  | C_ABS [@printer PP_helpers.const "ABS"]
  | C_ADD [@printer PP_helpers.const "ADD"]
  | C_SUB [@printer PP_helpers.const "SUB"]
  | C_MUL [@printer PP_helpers.const "MUL"]
  | C_EDIV [@printer PP_helpers.const "EDIV"]
  | C_DIV [@printer PP_helpers.const "DIV"]
  | C_MOD [@printer PP_helpers.const "MOD"]
  (* LOGIC *)
  | C_NOT [@printer PP_helpers.const "NOT"]
  | C_AND [@printer PP_helpers.const "AND"]
  | C_OR [@printer PP_helpers.const "OR"]
  | C_XOR [@printer PP_helpers.const "XOR"]
  | C_LSL [@printer PP_helpers.const "LSL"]
  | C_LSR [@printer PP_helpers.const "LSR"]
  (* COMPARATOR *)
  | C_EQ [@printer PP_helpers.const "EQ"]
  | C_NEQ [@printer PP_helpers.const "NEQ"]
  | C_LT [@printer PP_helpers.const "LT"]
  | C_GT [@printer PP_helpers.const "GT"]
  | C_LE [@printer PP_helpers.const "LE"]
  | C_GE [@printer PP_helpers.const "GE"]
  (* Bytes/ String *)
  | C_SIZE [@printer PP_helpers.const "SIZE"]
  | C_CONCAT [@printer PP_helpers.const "CONCAT"]
  | C_SLICE [@printer PP_helpers.const "SLICE"]
  | C_BYTES_PACK [@printer PP_helpers.const "BYTES_PACK"]
  | C_BYTES_UNPACK [@printer PP_helpers.const "BYTES_UNPACK"]
  | C_CONS [@printer PP_helpers.const "CONS"]
  (* Pair *)
  | C_PAIR [@printer PP_helpers.const "PAIR"]
  | C_CAR [@printer PP_helpers.const "CAR"]
  | C_CDR [@printer PP_helpers.const "CDR"]
  | C_TRUE [@printer PP_helpers.const "TRUE"]
  | C_FALSE [@printer PP_helpers.const "FALSE"]
  | C_LEFT [@printer PP_helpers.const "LEFT"]
  | C_RIGHT [@printer PP_helpers.const "RIGHT"]
  (* Set *)
  | C_SET_EMPTY [@printer PP_helpers.const "SET_EMPTY"]
  | C_SET_LITERAL [@printer PP_helpers.const "SET_LITERAL"]
  | C_SET_ADD [@printer PP_helpers.const "SET_ADD"]
  | C_SET_REMOVE [@printer PP_helpers.const "SET_REMOVE"]
  | C_SET_ITER [@printer PP_helpers.const "SET_ITER"]
  | C_SET_FOLD [@printer PP_helpers.const "SET_FOLD"]
  | C_SET_FOLD_DESC [@printer PP_helpers.const "SET_FOLD_DES"]
  | C_SET_MEM [@printer PP_helpers.const "SET_MEM"]
  | C_SET_UPDATE [@printer PP_helpers.const "SET_UPDATE"]
  (* List *)
  | C_LIST_EMPTY [@printer PP_helpers.const "LIST_EMPTY"]
  | C_LIST_LITERAL [@printer PP_helpers.const "LIST_LITERAL"]
  | C_LIST_ITER [@printer PP_helpers.const "LIST_ITER"]
  | C_LIST_MAP [@printer PP_helpers.const "LIST_MAP"]
  | C_LIST_FOLD [@printer PP_helpers.const "LIST_FOLD"]
  | C_LIST_FOLD_LEFT [@printer PP_helpers.const "LIST_FOLD_LEFT"]
  | C_LIST_FOLD_RIGHT [@printer PP_helpers.const "LIST_FOLD_RIGHT"]
  | C_LIST_HEAD_OPT [@printer PP_helpers.const "LIST_HEAD_OPT"]
  | C_LIST_TAIL_OPT [@printer PP_helpers.const "LIST_TAIL_OPT"]
  (* Maps *)
  | C_MAP [@printer PP_helpers.const "MAP"]
  | C_MAP_EMPTY [@printer PP_helpers.const "MAP_EMPTY"]
  | C_MAP_LITERAL [@printer PP_helpers.const "MAP_LITERAL"]
  | C_MAP_GET [@printer PP_helpers.const "MAP_GET"]
  | C_MAP_GET_FORCE [@printer PP_helpers.const "MAP_GET_FORCE"]
  | C_MAP_ADD [@printer PP_helpers.const "MAP_ADD"]
  | C_MAP_REMOVE [@printer PP_helpers.const "MAP_REMOVE"]
  | C_MAP_UPDATE [@printer PP_helpers.const "MAP_UPDATE"]
  | C_MAP_ITER [@printer PP_helpers.const "MAP_ITER"]
  | C_MAP_MAP [@printer PP_helpers.const "MAP_MAP"]
  | C_MAP_FOLD [@printer PP_helpers.const "MAP_FOLD"]
  | C_MAP_MEM [@printer PP_helpers.const "MAP_MEM"]
  | C_MAP_FIND [@printer PP_helpers.const "MAP_FIND"]
  | C_MAP_FIND_OPT [@printer PP_helpers.const "MAP_FIND_OPT"]
  | C_MAP_GET_AND_UPDATE [@printer PP_helpers.const "MAP_GET_AND_UPDATE"]
  (* Big Maps *)
  | C_BIG_MAP [@printer PP_helpers.const "BIG_MAP"]
  | C_BIG_MAP_EMPTY [@printer PP_helpers.const "BIG_MAP_EMPTY"]
  | C_BIG_MAP_LITERAL [@printer PP_helpers.const "BIG_MAP_LITERAL"]
  | C_BIG_MAP_GET_AND_UPDATE [@printer PP_helpers.const "BIG_MAP_GET_AND_UPDATE"]
  | C_BIG_MAP_IDENTIFIER [@printer PP_helpers.const "BIG_MAP_IDENTIFIER"]
  (* Crypto *)
  | C_SHA256 [@printer PP_helpers.const "SHA256"]
  | C_SHA512 [@printer PP_helpers.const "SHA512"]
  | C_BLAKE2b [@printer PP_helpers.const "BLAKE2b"]
  | C_HASH [@printer PP_helpers.const "HASH"]
  | C_HASH_KEY [@printer PP_helpers.const "HASH_KEY"]
  | C_CHECK_SIGNATURE [@printer PP_helpers.const "CHECK_SIGNATURE"]
  | C_CHAIN_ID [@printer PP_helpers.const "CHAIN_ID"]
  (* Blockchain *)
  | C_CALL [@printer PP_helpers.const "CALL"]
  | C_CONTRACT [@printer PP_helpers.const "CONTRACT"]
  | C_CONTRACT_OPT [@printer PP_helpers.const "CONTRACT_OPT"]
  | C_CONTRACT_ENTRYPOINT [@printer PP_helpers.const "CONTRACT_ENTRYPOINT"]
  | C_CONTRACT_ENTRYPOINT_OPT [@printer PP_helpers.const "CONTRACT_ENTRYPOINT_OPT"]
  | C_AMOUNT [@printer PP_helpers.const "AMOUNT"]
  | C_BALANCE [@printer PP_helpers.const "BALANCE"]
  | C_SOURCE [@printer PP_helpers.const "SOURCE"]
  | C_SENDER [@printer PP_helpers.const "SENDER"]
  | C_ADDRESS [@printer PP_helpers.const "ADDRESS"]
  | C_SELF [@printer PP_helpers.const "SELF"]
  | C_SELF_ADDRESS [@printer PP_helpers.const "SELF_ADDRESS"]
  | C_IMPLICIT_ACCOUNT [@printer PP_helpers.const "IMPLICIT_ACCOUNT"]
  | C_SET_DELEGATE [@printer PP_helpers.const "SET_DELEGATE"]
  | C_CREATE_CONTRACT [@printer PP_helpers.const "CREATE_CONTRACT"]
  | C_CONVERT_TO_LEFT_COMB [@printer PP_helpers.const "CONVERT_TO_LEFT_COMB"]
  | C_CONVERT_TO_RIGHT_COMB [@printer PP_helpers.const "CONVERT_TO_RIGHT_COMB"]
  | C_CONVERT_FROM_LEFT_COMB [@printer PP_helpers.const "CONVERT_FROM_LEFT_COMB"]
  | C_CONVERT_FROM_RIGHT_COMB [@printer PP_helpers.const "CONVERT_FROM_RIGHT_COMB"]
  (* Tests - ligo interpreter only *)
  | C_TEST_ORIGINATE [@printer PP_helpers.const "TEST_ORIGINATE"]
  | C_TEST_GET_STORAGE [@printer PP_helpers.const "TEST_GET_STORAGE"]
  | C_TEST_GET_STORAGE_OF_ADDRESS [@printer PP_helpers.const "TEST_GET_STORAGE_OF_ADDRESS"]
  | C_TEST_GET_BALANCE [@printer PP_helpers.const "TEST_GET_BALANCE"]
  | C_TEST_SET_NOW [@printer PP_helpers.const "TEST_SET_NOW"]
  | C_TEST_SET_SOURCE [@printer PP_helpers.const "TEST_SET_SOURCE"]
  | C_TEST_SET_BAKER [@printer PP_helpers.const "TEST_SET_BAKER"]
  | C_TEST_EXTERNAL_CALL_TO_CONTRACT [@printer PP_helpers.const "TEST_EXTERNAL_CALL_TO_CONTRACT"]
  | C_TEST_EXTERNAL_CALL_TO_CONTRACT_EXN [@printer PP_helpers.const "TEST_EXTERNAL_CALL_TO_CONTRACT_EXN"]
  | C_TEST_EXTERNAL_CALL_TO_ADDRESS [@printer PP_helpers.const "TEST_EXTERNAL_CALL_TO_ADDRESS"]
  | C_TEST_EXTERNAL_CALL_TO_ADDRESS_EXN [@printer PP_helpers.const "TEST_EXTERNAL_CALL_TO_ADDRESS_EXN"]
  | C_TEST_MICHELSON_EQUAL [@printer PP_helpers.const "TEST_MICHELSON_EQUAL"]
  | C_TEST_GET_NTH_BS [@printer PP_helpers.const "TEST_GET_NTH_BS"]
  | C_TEST_LOG [@printer PP_helpers.const "TEST_LOG"]
  | C_TEST_COMPILE_EXPRESSION [@printer PP_helpers.const "TEST_COMPILE_EXPRESSION"]
  | C_TEST_COMPILE_EXPRESSION_SUBST [@printer PP_helpers.const "TEST_COMPILE_EXPRESSION_SUBST"]
  | C_TEST_STATE_RESET [@printer PP_helpers.const "TEST_STATE_RESET"]
  | C_TEST_LAST_ORIGINATIONS [@printer PP_helpers.const "TEST_LAST_ORIGINATIONS"]
  | C_TEST_COMPILE_META_VALUE [@printer PP_helpers.const "TEST_COMPILE_META_VALUE"]
  | C_TEST_RUN [@printer PP_helpers.const "TEST_RUN"]
  | C_TEST_EVAL [@printer PP_helpers.const "TEST_EVAL"]
  | C_TEST_COMPILE_CONTRACT [@printer PP_helpers.const "TEST_COMPILE_CONTRACT"]
  | C_TEST_TO_CONTRACT [@printer PP_helpers.const "TEST_TO_CONTRACT"]
  | C_TEST_TO_ENTRYPOINT [@printer PP_helpers.const "TEST_TO_ENTRYPOINT"]
  | C_TEST_ORIGINATE_FROM_FILE [@printer PP_helpers.const "TEST_ORIGINATE_FROM_FILE"]
  (* New with EDO*)
  | C_SHA3 [@printer PP_helpers.const "SHA3"]
  | C_KECCAK [@printer PP_helpers.const "KECCAK"]
  | C_LEVEL [@printer PP_helpers.const "LEVEL"]
  | C_VOTING_POWER [@printer PP_helpers.const "VOTING_POWER"]
  | C_TOTAL_VOTING_POWER [@printer PP_helpers.const "TOTAL_VOTING_POWER"]
  | C_TICKET [@printer PP_helpers.const "TICKET"]
  | C_READ_TICKET [@printer PP_helpers.const "READ_TICKET"]
  | C_SPLIT_TICKET [@printer PP_helpers.const "SPLIT_TICKET"]
  | C_JOIN_TICKET [@printer PP_helpers.const "JOIN_TICKET"]
  | C_PAIRING_CHECK [@printer PP_helpers.const "PAIRING_CHECK"]
  | C_SAPLING_VERIFY_UPDATE [@printer PP_helpers.const "SAPLING_VERIFY_UPDATE"]
  | C_SAPLING_EMPTY_STATE [@printer PP_helpers.const "SAPLING_EMPTY_STATE"]
  (* JsLIGO *)
  | C_POLYMORPHIC_ADD [@printer PP_helpers.const "C_POLYMORPHIC_ADD"]
[@@deriving enum, yojson, show { with_path = false } ]

type deprecated = {
  name : string ;
  const : constant' ;
}

type rich_constant =
  | Deprecated of deprecated
  | Const of constant'
