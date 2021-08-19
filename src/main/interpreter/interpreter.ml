open Trace
open Ligo_interpreter.Types
open Ligo_interpreter.Combinators
include Ast_typed.Types
module Env = Ligo_interpreter.Environment

type interpreter_error = Errors.interpreter_error

let check_value value =
  let open Monad in
  match value with
  | V_Func_val {orig_lambda} ->
     call @@ Check_obj_ligo orig_lambda
  | _ -> return ()

let monad_option error = fun v ->
    let open Monad in
    match v with
      None -> fail error
    | Some s -> return s

let rec apply_comparison : Location.t -> calltrace -> Ast_typed.constant' -> value list -> value Monad.t =
  fun loc calltrace c operands ->
    let open Monad in
    match (c,operands) with
    | ( comp , [ V_Ct (C_int a'      ) ; V_Ct (C_int b'      ) ] )
    | ( comp , [ V_Ct (C_mutez a'    ) ; V_Ct (C_mutez b'    ) ] )
    | ( comp , [ V_Ct (C_timestamp a') ; V_Ct (C_timestamp b') ] )
    | ( comp , [ V_Ct (C_nat a'      ) ; V_Ct (C_nat b'      ) ] ) ->
      let>> i = Int_compare_wrapped (a', b') in
      let>> cmpres = Int_of_int i in
      let>> cmpres = Int_compare (cmpres, Ligo_interpreter.Int_repr_copied.zero) in
      let* x = match comp with
        | C_EQ -> return (cmpres = 0)
        | C_NEQ -> return (cmpres <> 0)
        | C_LT -> return (cmpres < 0)
        | C_LE -> return (cmpres <= 0)
        | C_GT -> return (cmpres > 0)
        | C_GE -> return (cmpres >= 0)
        | _ -> fail @@ Errors.meta_lang_eval loc calltrace "Not comparable"
      in
      return @@ v_bool x
    | ( comp     , [ V_Ct (C_bool b ) ; V_Ct (C_bool a ) ] ) ->
      let cmpres = Bool.compare b a in
      let* x = match comp with
        | C_EQ -> return (cmpres = 0)
        | C_NEQ -> return (cmpres <> 0)
        | C_LT -> return (cmpres < 0)
        | C_LE -> return (cmpres <= 0)
        | C_GT -> return (cmpres > 0)
        | C_GE -> return (cmpres >= 0)
        | _ -> fail @@ Errors.meta_lang_eval loc calltrace "Not comparable"
      in
      return @@ v_bool x
    | ( comp     , [ V_Ct (C_address b ) ; V_Ct (C_address a ) ] ) ->
      let cmpres = Tezos_state.compare_account_ b a in
      let* x = match comp with
        | C_EQ -> return (cmpres = 0)
        | C_NEQ -> return (cmpres <> 0)
        | C_LT -> return (cmpres < 0)
        | C_LE -> return (cmpres <= 0)
        | C_GT -> return (cmpres > 0)
        | C_GE -> return (cmpres >= 0)
        | _ -> fail @@ Errors.meta_lang_eval loc calltrace "Not comparable"
      in
      return @@ v_bool x
    | ( comp     , [ V_Ct (C_key_hash b ) ; V_Ct (C_key_hash a ) ] ) ->
      let cmpres = Tezos_crypto.Signature.Public_key_hash.compare b a in
      let* x = match comp with
        | C_EQ -> return (cmpres = 0)
        | C_NEQ -> return (cmpres <> 0)
        | C_LT -> return (cmpres < 0)
        | C_LE -> return (cmpres <= 0)
        | C_GT -> return (cmpres > 0)
        | C_GE -> return (cmpres >= 0)
        | _ -> fail @@ Errors.meta_lang_eval loc calltrace "Not comparable"
      in
      return @@ v_bool x
    | ( comp     , [ V_Ct (C_unit ) ; V_Ct (C_unit ) ] ) ->
      let* x = match comp with
        | C_EQ -> return true
        | C_NEQ -> return false
        | C_LT -> return false
        | C_LE -> return true
        | C_GT -> return false
        | C_GE -> return true
        | _ -> fail @@ Errors.meta_lang_eval loc calltrace "Not comparable"
      in
      return @@ v_bool x
    | ( comp     , [ V_Ct (C_string a'  ) ; V_Ct (C_string b'  ) ] ) ->
      let* f_op = match comp with
        | C_EQ -> return @@ fun a b -> (String.compare a b = 0)
        | C_NEQ -> return @@ fun a b -> (String.compare a b != 0)
        | C_LT -> return @@ fun a b -> (String.compare a b < 0)
        | C_LE -> return @@ fun a b -> (String.compare a b <= 0)
        | C_GT -> return @@ fun a b -> (String.compare a b > 0)
        | C_GE -> return @@ fun a b -> (String.compare a b >= 0)
        | _ -> fail @@ Errors.meta_lang_eval loc calltrace "Not comparable" in
      Monad.return @@ v_bool (f_op a' b')

    | ( comp     , [ V_Ct (C_bytes a'  ) ; V_Ct (C_bytes b'  ) ] ) ->
      let* f_op = match comp with
        | C_EQ -> return @@ fun a b -> (Bytes.compare a b = 0)
        | C_NEQ -> return @@ fun a b -> (Bytes.compare a b != 0)
        | C_LT -> return @@ fun a b -> (Bytes.compare a b < 0)
        | C_LE -> return @@ fun a b -> (Bytes.compare a b <= 0)
        | C_GT -> return @@ fun a b -> (Bytes.compare a b > 0)
        | C_GE -> return @@ fun a b -> (Bytes.compare a b >= 0)
        | _ -> fail @@ Errors.meta_lang_eval loc calltrace "Not comparable" in
      Monad.return @@ v_bool (f_op a' b')
    | ( comp , [ V_Construct (ctor_a, args_a) ; V_Construct (ctor_b, args_b ) ] ) -> (
       match comp with
       | C_EQ ->
          if (String.equal ctor_a ctor_b) then
            let* r = apply_comparison loc calltrace c [ args_a ;  args_b ] in
            Monad.return @@ v_bool @@ is_true r
          else
            Monad.return @@ v_bool false
       | C_NEQ ->
          if (not (String.equal ctor_a ctor_b)) then
            Monad.return @@ v_bool true
          else
            let* r = apply_comparison loc calltrace c [ args_a ;  args_b ] in
            Monad.return @@ v_bool @@ is_true r
       | _ -> fail @@ Errors.meta_lang_eval loc calltrace "Not comparable"
    )
    | ( _ , l ) ->
       print_endline (Format.asprintf "%a" (PP_helpers.list_sep_d Ligo_interpreter.PP.pp_value) l);
       fail @@ Errors.meta_lang_eval loc calltrace "Not comparable"

let rec apply_operator : Location.t -> calltrace -> Ast_typed.type_expression -> env -> Ast_typed.constant' -> (value * Ast_typed.type_expression) list -> value Monad.t =
  fun loc calltrace expr_ty env c operands ->
  let open Monad in
  let types = List.map ~f:snd operands in
  let operands = List.map ~f:fst operands in
  let return_ct v = return @@ V_Ct v in
  let return_none () = return @@ v_none () in
  let return_some v = return @@ v_some v in
  ( match (c,operands) with
    (* nullary *)
    | ( C_NONE , [] ) -> return_none ()
    | ( C_UNIT , [] ) -> return @@ V_Ct C_unit
    | ( C_NIL  , [] ) -> return @@ V_List []
    (* unary *)
    | ( C_SIZE   , [(V_Set l | V_List l)] ) -> return_ct @@ C_nat (Z.of_int @@ List.length l)
    | ( C_SIZE   , [ V_Map l            ] ) -> return_ct @@ C_nat (Z.of_int @@ List.length l)
    | ( C_SIZE   , [ V_Ct (C_string s ) ] ) -> return_ct @@ C_nat (Z.of_int @@ String.length s)
    | ( C_SIZE   , [ V_Ct (C_bytes b  ) ] ) -> return_ct @@ C_nat (Z.of_int @@ Bytes.length b)
    | ( C_NOT    , [ V_Ct (C_bool a'  ) ] ) -> return_ct @@ C_bool (not a')
    | ( C_INT    , [ V_Ct (C_nat a')    ] ) -> return_ct @@ C_int a'
    | ( C_ABS    , [ V_Ct (C_int a')    ] ) -> return_ct @@ C_nat (Z.abs a')
    | ( C_NEG    , [ V_Ct (C_int a')    ] ) -> return_ct @@ C_int (Z.neg a')
    | ( C_SOME   , [ v                  ] ) -> return_some v
    | ( C_IS_NAT , [ V_Ct (C_int a')    ] ) ->
      if a' > Z.zero then return_some @@ V_Ct (C_nat a')
      else return_none ()
    | ( C_FOLD_CONTINUE  , [ v ] ) -> return @@ v_pair (v_bool true  , v)
    | ( C_FOLD_STOP      , [ v ] ) -> return @@ v_pair (v_bool false , v)
    | ( C_ASSERTION , [ v ] ) ->
      if (is_true v) then return_ct @@ C_unit
      else fail @@ Errors.meta_lang_eval loc calltrace "Failed assertion"
    | C_MAP_FIND_OPT , [ k ; V_Map l ] -> ( match List.Assoc.find ~equal:Caml.(=) l k with
      | Some v -> return @@ v_some v
      | None -> return @@ v_none ()
    )
    | C_MAP_FIND_OPT , [ k ; V_BigMap (id, kvs) ] ->
       (match List.Assoc.find kvs ~equal:Caml.(=) k with
        | Some (Some v) -> return @@ v_some v
        | Some None -> return @@ v_none ()
        | None ->
           let* key_ty, val_ty =
             monad_option (Errors.generic_error loc "Not a big-map") @@ Ast_typed.get_t_big_map (List.nth_exn types 1) in
           let>> typed_exp = Get_big_map (loc, calltrace, key_ty, val_ty, k, id) in
           let* v = eval_ligo typed_exp calltrace env in
           return @@ v
       )
    | C_MAP_FIND , [ k ; V_Map l ] -> ( match List.Assoc.find ~equal:Caml.(=) l k with
      | Some v -> return @@ v
      | None -> fail @@ Errors.meta_lang_eval loc calltrace (Predefined.Tree_abstraction.pseudo_module_to_string c)
    )
    (* binary *)
    | ( (C_EQ | C_NEQ | C_LT | C_LE | C_GT | C_GE) , _ ) -> apply_comparison loc calltrace c operands
    | ( C_SUB    , [ V_Ct (C_int a' | C_nat a') ; V_Ct (C_int b' | C_nat b') ] ) -> return_ct @@ C_int (Z.sub a' b')
    | ( C_CONS   , [ v                  ; V_List vl          ] ) -> return @@ V_List (v::vl)
    | ( C_ADD    , [ V_Ct (C_int a'  )  ; V_Ct (C_int b'  )  ] )
    | ( C_ADD    , [ V_Ct (C_nat a'  )  ; V_Ct (C_int b'  )  ] )
    | ( C_ADD    , [ V_Ct (C_int a'  )  ; V_Ct (C_nat b'  )  ] ) -> let>> r = Int_add (a',b') in return_ct (C_int r)
    | ( C_ADD    , [ V_Ct (C_nat a'  )  ; V_Ct (C_nat b'  )  ] ) -> let>> r = Int_add_n (a',b') in return_ct (C_nat r)
    | ( C_MUL    , [ V_Ct (C_int a'  )  ; V_Ct (C_int b'  )  ] )
    | ( C_MUL    , [ V_Ct (C_nat a'  )  ; V_Ct (C_int b'  )  ] )
    | ( C_MUL    , [ V_Ct (C_int a'  )  ; V_Ct (C_nat b'  )  ] ) -> let>> r = Int_mul (a',b') in return_ct (C_int r)
    | ( C_MUL    , [ V_Ct (C_nat a'  )  ; V_Ct (C_nat b'  )  ] ) -> let>> r = Int_mul_n (a',b') in return_ct (C_nat r)
    | ( C_MUL    , [ V_Ct (C_nat a'  )  ; V_Ct (C_mutez b')  ] ) -> let>> r = Int_mul_n (a',b') in return_ct (C_mutez r)
    | ( C_MUL    , [ V_Ct (C_mutez a')  ; V_Ct (C_nat b'  )  ] ) -> let>> r = Int_mul_n (a',b') in return_ct (C_mutez r)
    | ( C_DIV    , [ V_Ct (C_int a'  )  ; V_Ct (C_int b'  )  ] )
    | ( C_DIV    , [ V_Ct (C_int a'  )  ; V_Ct (C_nat b'  )  ] )
    | ( C_DIV    , [ V_Ct (C_nat a'  )  ; V_Ct (C_int b'  )  ] ) ->
      let>> a = Int_ediv (a',b') in
      begin
        match a with
        | Some (res,_) -> return_ct @@ C_int res
        | None -> fail @@ Errors.meta_lang_eval loc calltrace "Dividing by zero"
      end
    | ( C_DIV    , [ V_Ct (C_nat a')  ; V_Ct (C_nat b')  ] ) ->
      let>> a = Int_ediv_n (a',b') in
      begin
        match a with
        | Some (res,_) -> return_ct @@ C_nat res
        | None -> fail @@ Errors.meta_lang_eval loc calltrace "Dividing by zero"
      end
    | ( C_DIV    , [ V_Ct (C_mutez a')  ; V_Ct (C_mutez b')  ] ) ->
      let>> a = Int_ediv_n (a',b') in
      begin
        match a with
        | Some (res,_) -> return_ct @@ C_nat res
        | None -> fail @@ Errors.meta_lang_eval loc calltrace "Dividing by zero"
      end
    | ( C_DIV    , [ V_Ct (C_mutez a')  ; V_Ct (C_nat b')  ] ) ->
      let>> a = Int_ediv_n (a',b') in
      begin
        match a with
        | Some (res,_) -> return_ct @@ C_mutez res
        | None -> fail @@ Errors.meta_lang_eval loc calltrace "Dividing by zero"
      end
    | ( C_MOD    , [ V_Ct (C_int a')    ; V_Ct (C_int b')    ] )
    | ( C_MOD    , [ V_Ct (C_int a')    ; V_Ct (C_nat b')    ] )
    | ( C_MOD    , [ V_Ct (C_nat a')    ; V_Ct (C_int b')    ] ) -> (
      let>> a = Int_ediv (a',b') in
      match a with
      | Some (_,r) -> return_ct @@ C_nat r
      | None -> fail @@ Errors.meta_lang_eval loc calltrace "Dividing by zero"
    )
    | ( C_MOD    , [ V_Ct (C_nat a')    ; V_Ct (C_nat b')    ] ) -> (
      let>> a = Int_ediv_n (a',b') in
      match a with
      | Some (_,r) -> return_ct @@ C_nat r
      | None -> fail @@ Errors.meta_lang_eval loc calltrace "Dividing by zero"
    )
    | ( C_CONCAT , [ V_Ct (C_string a') ; V_Ct (C_string b') ] ) -> return_ct @@ C_string (a' ^ b')
    | ( C_CONCAT , [ V_Ct (C_bytes a' ) ; V_Ct (C_bytes b' ) ] ) -> return_ct @@ C_bytes  (Bytes.cat a' b')
    | ( C_OR     , [ V_Ct (C_bool a'  ) ; V_Ct (C_bool b'  ) ] ) -> return_ct @@ C_bool   (a' || b')
    | ( C_AND    , [ V_Ct (C_bool a'  ) ; V_Ct (C_bool b'  ) ] ) -> return_ct @@ C_bool   (a' && b')
    | ( C_XOR    , [ V_Ct (C_bool a'  ) ; V_Ct (C_bool b'  ) ] ) -> return_ct @@ C_bool   ( (a' || b') && (not (a' && b')) )
    | ( C_LIST_EMPTY, []) -> return @@ V_List ([])
    | ( C_LIST_MAP , [ V_Func_val {arg_binder ; body ; env}  ; V_List (elts) ] ) ->
      let* elts =
        Monad.bind_map_list
          (fun elt ->
            let env' = Env.extend env (arg_binder,elt) in
            eval_ligo body calltrace env')
          elts
      in
      return (V_List elts)
    | ( C_MAP_MAP , [ V_Func_val {arg_binder ; body ; env}  ; V_Map (elts) ] ) ->
      let* elts =
        Monad.bind_map_list
          (fun (k,v) ->
            let env' = Env.extend env (arg_binder,v_pair (k,v)) in
            let* v' = eval_ligo body calltrace env' in
            return @@ (k,v')
          )
          elts
      in
      return (V_Map elts)
    | ( C_LIST_ITER , [ V_Func_val {arg_binder ; body ; env}  ; V_List (elts) ] ) ->
      Monad.bind_fold_list
        (fun _ elt ->
          let env' = Env.extend env (arg_binder,elt) in
          eval_ligo body calltrace env'
        )
        (V_Ct C_unit) elts
    | ( C_MAP_ITER , [ V_Func_val {arg_binder ; body ; env}  ; V_Map (elts) ] ) ->
      Monad.bind_fold_list
        (fun _ kv ->
          let env' = Env.extend env (arg_binder,v_pair kv) in
          eval_ligo body calltrace env'
        )
        (V_Ct C_unit) elts
    | ( C_FOLD_WHILE , [ V_Func_val {arg_binder ; body ; env}  ; init ] ) -> (
      let rec aux b el =
        let env' = Env.extend env (arg_binder, el) in
        let* res = eval_ligo body calltrace env' in
        let (b',el') = try Option.value_exn (extract_fold_while_result res) with _ -> (failwith "bad pair") in
        if b then aux b' el' else return el' in
      aux true init
    )
    (* tertiary *)
    | ( C_SLICE , [ V_Ct (C_nat st) ; V_Ct (C_nat ed) ; V_Ct (C_string s) ] ) ->
      (*TODO : allign with tezos*)
      return @@ V_Ct (C_string (String.sub s (Z.to_int st) (Z.to_int ed)))
    | ( C_LIST_FOLD , [ V_Func_val {arg_binder ; body ; env}  ; V_List elts ; init ] ) ->
      Monad.bind_fold_list
        (fun prev elt ->
          let fold_args = v_pair (prev,elt) in
          let env' = Env.extend env (arg_binder,  fold_args) in
          eval_ligo body calltrace env'
        )
        init elts
    | ( C_BIG_MAP_EMPTY , []) -> return @@ V_Map ([])
    | ( C_BIG_MAP_IDENTIFIER , [ V_Ct (C_nat n) ]) ->
       return @@ V_BigMap (n, [])
    | ( C_MAP_EMPTY , []) -> return @@ V_Map ([])
    | ( C_MAP_FOLD , [ V_Func_val {arg_binder ; body ; env}  ; V_Map kvs ; init ] ) ->
      Monad.bind_fold_list
        (fun prev kv ->
          let fold_args = v_pair (prev, v_pair kv) in
          let env' = Env.extend env (arg_binder,  fold_args) in
          eval_ligo body calltrace env'
        )
        init kvs
    | ( C_MAP_MEM , [k ; V_Map kvs]) -> return @@ v_bool (List.Assoc.mem ~equal:Caml.(=) kvs k)
    | ( C_MAP_MEM , [k ; V_BigMap (m, kvs) ]) ->
          ( match List.Assoc.find kvs ~equal:(=) k with
            | Some (Some _) -> return @@ v_bool true
            | Some None -> return @@ v_bool false
            | None ->
               let* key_ty, val_ty =
                 monad_option (Errors.generic_error loc "Not a big-map") @@ Ast_typed.get_t_big_map (List.nth_exn types 1) in
               let>> b = Mem_big_map (loc, calltrace, key_ty, val_ty, k, m) in
               return @@ v_bool b
          )
    | ( C_MAP_ADD , [ k ; v ; V_Map kvs] ) -> return (V_Map ((k,v) :: List.Assoc.remove ~equal:Caml.(=) kvs k))
    | ( C_MAP_ADD , [ k ; v ; V_BigMap (id, kvs) ]) -> return (V_BigMap (id, (k, Some v) :: kvs))
    | ( C_MAP_REMOVE , [ k ; V_Map kvs] ) -> return @@ V_Map (List.Assoc.remove ~equal:Caml.(=) kvs k)
    | ( C_MAP_REMOVE , [ k ; V_BigMap (id, kvs) ] ) -> return (V_BigMap (id, (k, None) :: kvs))
    | ( C_MAP_UPDATE , [ k ; V_Construct (option,v) ; V_Map kvs] ) -> (match option with
      | "Some" -> return @@ V_Map ((k,v)::(List.Assoc.remove ~equal:Caml.(=) kvs k))
      | "None" -> return @@ V_Map (List.Assoc.remove ~equal:Caml.(=) kvs k)
      | _ -> assert false
    )
    | ( C_MAP_UPDATE , [ k ; V_Construct (option,v) ; V_BigMap (id, kvs) ] ) -> (match option with
         | "Some" -> return @@ V_BigMap (id, (k, Some v)::(List.Assoc.remove ~equal:Caml.(=) kvs k))
         | "None" -> return @@ V_BigMap (id, (k, None)::(List.Assoc.remove ~equal:Caml.(=) kvs k))
         | _ -> assert false
    )
    | ( C_SET_EMPTY, []) -> return @@ V_Set ([])
    | ( C_SET_ADD , [ v ; V_Set l ] ) -> return @@ V_Set (List.dedup_and_sort ~compare (v::l))
    | ( C_SET_FOLD , [ V_Func_val {arg_binder ; body ; env}  ; V_Set elts ; init ] ) ->
      Monad.bind_fold_list
        (fun prev elt ->
          let fold_args = v_pair (prev,elt) in
          let env' = Env.extend env (arg_binder, fold_args) in
          eval_ligo body calltrace env'
        )
        init elts
    | ( C_SET_ITER , [ V_Func_val {arg_binder ; body ; env}  ; V_Set (elts) ] ) ->
      Monad.bind_fold_list
        (fun _ elt ->
          let env' = Env.extend env (arg_binder,elt) in
          eval_ligo body calltrace env'
        )
        (V_Ct C_unit) elts
    | ( C_SET_MEM    , [ v ; V_Set (elts) ] ) -> return @@ v_bool (List.mem ~equal:Caml.(=) elts v)
    | ( C_SET_REMOVE , [ v ; V_Set (elts) ] ) -> return @@ V_Set (List.filter ~f:(fun el -> not (el = v)) elts)
    | ( C_ADDRESS , [ V_Ct (C_contract { address }) ] ) ->
      return (V_Ct (C_address address))
    | ( C_TRUE , [] ) -> return @@ v_bool true
    | ( C_FALSE , [] ) -> return @@ v_bool false
    (*
    >>>>>>>>
      Test operators
    >>>>>>>>
    *)
    | ( C_TEST_COMPILE_EXPRESSION_SUBST, [ file_opt ; V_Ligo (syntax,ligo_exp) ; subst ] ) ->
      let>> code = Compile_expression (loc, calltrace, file_opt, syntax, ligo_exp, Some subst) in
      return code
    | ( C_TEST_COMPILE_EXPRESSION, [ file_opt ; V_Ligo (syntax,ligo_exp) ] ) ->
      let>> code = Compile_expression (loc, calltrace, file_opt, syntax, ligo_exp, None) in
      return code
    | ( C_TEST_ORIGINATE_FROM_FILE, [ V_Ct (C_string source_file) ; V_Ct (C_string entryp) ; storage ; V_Ct ( C_mutez amt ) ] ) ->
      let>> (code,size) = Compile_contract_from_file (source_file,entryp) in
      let>> addr = Inject_script (loc, calltrace, code, storage, amt) in
      return @@ V_Record (LMap.of_list [ (Label "0", addr) ; (Label "1", code) ; (Label "2", size) ])
    | ( C_TEST_EXTERNAL_CALL_TO_ADDRESS_EXN , [ (V_Ct (C_address address)) ; V_Michelson (Ty_code (param,_,_)) ; V_Ct ( C_mutez amt ) ] ) -> (
      let contract = { address; entrypoint = None } in
      let>> err_opt = External_call (loc,calltrace,contract,param,amt) in
      match err_opt with
      | None -> return_ct C_unit
      | Some e -> fail @@ Errors.target_lang_error loc calltrace e
    )
    | ( C_TEST_EXTERNAL_CALL_TO_ADDRESS , [ (V_Ct (C_address address)) ; V_Michelson (Ty_code (param,_,_)) ; V_Ct ( C_mutez amt ) ] ) -> (
      let contract = { address; entrypoint = None } in
      let>> err_opt = External_call (loc,calltrace,contract,param,amt) in
      match err_opt with
      | None -> return (LC.v_ctor "Success" (LC.v_unit ()))
      | Some e ->
        let>> a = State_error_to_value e in
        return a
    )
    | ( C_TEST_SET_NOW , [ V_Ct (C_timestamp t) ] ) ->
      let>> () = Set_now (loc,calltrace,t) in
      return_ct C_unit
    | ( C_TEST_SET_SOURCE , [ addr ] ) ->
      let>> () = Set_source addr in
      return_ct C_unit
    | ( C_TEST_SET_BAKER , [ addr ] ) ->
      let>> () = Set_baker addr in
      return_ct C_unit
    | ( C_TEST_GET_STORAGE_OF_ADDRESS , [ addr ] ) ->
      let>> storage = Get_storage_of_address (loc, calltrace, addr) in
      return storage
    | ( C_TEST_GET_BALANCE , [ addr ] ) ->
      let>> balance = Get_balance (loc, calltrace, addr) in
      return balance
    | ( C_TEST_MICHELSON_EQUAL , [ a ; b ] ) ->
      let>> b = Michelson_equal (loc,a,b) in
      return_ct (C_bool b)
    | ( C_TEST_LOG , [ v ]) ->
      let () = Format.printf "%a\n" Ligo_interpreter.PP.pp_value v in
      return_ct C_unit
    | ( C_TEST_BOOTSTRAP_CONTRACT , [ V_Ct (C_mutez z) ; contract ; storage ] ) ->
       let contract_ty = List.nth_exn types 1 in
       let storage_ty = List.nth_exn types 2 in
       let>> code = Compile_contract (loc, contract, contract_ty) in
       let>> storage = Eval (loc, storage, storage_ty) in
       let>> () = Bootstrap_contract ((Z.to_int z), code, storage, contract_ty) in
       return_ct C_unit
    | ( C_TEST_NTH_BOOTSTRAP_CONTRACT , [ V_Ct (C_nat n) ] ) ->
       let n = Z.to_int n in
       let>> address = Nth_bootstrap_contract n in
       return_ct (C_address address)
    | ( C_TEST_STATE_RESET , [ n ; amts ] ) ->
      let>> () = Reset_state (loc,calltrace,n,amts) in
      return_ct C_unit
    | ( C_TEST_GET_NTH_BS , [ n ] ) ->
      let>> x = Get_bootstrap (loc,n) in
      return x
    | ( C_TEST_LAST_ORIGINATIONS , [ _ ] ) ->
      let>> x = Get_last_originations () in
      return x
    | ( C_TEST_MUTATE_EXPRESSION , [ V_Ct (C_nat n); V_Ligo (syntax,ligo_exp) ] ) ->
      let>> x = Mutate_expression (n,syntax,ligo_exp) in
      return (V_Ligo x)
    | ( C_TEST_MUTATE_COUNT , [ V_Ligo (syntax,ligo_exp) ] ) ->
      let>> x = Mutate_count (syntax,ligo_exp) in
      return x
    | ( C_TEST_MUTATE_VALUE , [ V_Ct (C_nat n); v ] ) -> (
      let* () = check_value v in
      let value_ty = List.nth_exn types 1 in
      let>> v = Mutate_some_value (loc,n,v, value_ty) in
      match v with
      | None ->
         return (v_none ())
      | Some (e, m) ->
         let* v = eval_ligo e calltrace env in
         return @@ (v_some (V_Record (LMap.of_list [ (Label "0", v) ; (Label "1", V_Mutation m) ]))))
    | ( C_TEST_MUTATION_TEST , [ v; tester ] ) ->
      let* () = check_value v in
      let value_ty = List.nth_exn types 0 in
      let>> l = Mutate_all_value (loc,v,value_ty) in
      let* r = iter_while (fun (e, m) ->
        let* v = eval_ligo e calltrace env in
        let r =  match tester with
          | V_Func_val {arg_binder ; body ; env; rec_name = None ; orig_lambda } ->
             let in_ty, _ = Ast_typed.get_t_function_exn orig_lambda.type_expression in
             let f_env' = Env.extend ~ast_type:in_ty env (arg_binder, v) in
             eval_ligo body (loc :: calltrace) f_env'
          | V_Func_val {arg_binder ; body ; env; rec_name = Some fun_name; _} ->
             let f_env' = Env.extend env (arg_binder, v) in
             let f_env'' = Env.extend f_env' (fun_name, tester) in
             eval_ligo body (loc :: calltrace) f_env''
          | _ -> fail @@ Errors.generic_error loc "Trying to apply on something that is not a function?" in
        Monad.try_or (let* v = r in return (Some (v, m))) (return None)) l in
      (match r with
       | None -> return (v_none ())
       | Some (v, m) -> return (v_some (V_Record (LMap.of_list [ (Label "0", v) ; (Label "1", V_Mutation m) ]))))
    | ( C_TEST_MUTATION_TEST_ALL , [ v; tester ] ) ->
      let* () = check_value v in
      let value_ty = List.nth_exn types 0 in
      let>> l = Mutate_all_value (loc,v,value_ty) in
      let* r = bind_map_list (fun (e, m) ->
        let* v = eval_ligo e calltrace env in
        let r =  match tester with
          | V_Func_val {arg_binder ; body ; env; rec_name = None ; orig_lambda } ->
             let in_ty, _ = Ast_typed.get_t_function_exn orig_lambda.type_expression in
             let f_env' = Env.extend ~ast_type:in_ty env (arg_binder, v) in
             eval_ligo body (loc :: calltrace) f_env'
          | V_Func_val {arg_binder ; body ; env; rec_name = Some fun_name; _} ->
             let f_env' = Env.extend env (arg_binder, v) in
             let f_env'' = Env.extend f_env' (fun_name, tester) in
             eval_ligo body (loc :: calltrace) f_env''
          | _ -> fail @@ Errors.generic_error loc "Trying to apply on something that is not a function?" in
        Monad.try_or (let* v = r in return (Some (v, m))) (return None)) l in
      let r = List.map ~f:(fun (v, m) -> V_Record (LMap.of_list [ (Label "0", v) ; (Label "1", V_Mutation m) ])) @@ List.filter_opt r in
      return (V_List r)
    | ( C_TEST_TO_CONTRACT , [ addr ] ) ->
       let contract_ty = List.nth_exn types 0 in
       let>> code = To_contract (loc, addr, None, contract_ty) in
       return code
    | ( C_TEST_TO_ENTRYPOINT , [ V_Ct (C_string ent) ; addr ] ) ->
       let contract_ty = List.nth_exn types 0 in
       let>> code = To_contract (loc, addr, Some ent, contract_ty) in
       return code
    | ( C_TEST_TO_TYPED_ADDRESS , [ V_Ct (C_contract { address; _ }) ] ) ->
       let>> () = Check_storage_address (loc, address, expr_ty) in
       let addr = LT.V_Ct ( C_address address ) in
       return addr
    | ( C_TEST_RUN , [ V_Func_val f ; v ] ) ->
       let* () = check_value (V_Func_val f) in
       let* () = check_value v in
       let>> code = Run (loc, f, v) in
       return code
    | ( C_TEST_EVAL , [ v ] )
    | ( C_TEST_COMPILE_META_VALUE , [ v ] ) ->
       let* () = check_value v in
       let value_ty = List.nth_exn types 0 in
       let>> code = Eval (loc, v, value_ty) in
       return code
    | ( C_TEST_GET_STORAGE , [ addr ] ) ->
       let typed_address_ty = List.nth_exn types 0 in
       let storage_ty = match Ast_typed.get_t_typed_address typed_address_ty with
         | Some (_, storage_ty) -> storage_ty
         | _ -> failwith "Expecting typed_address" in
       let>> typed_exp = Get_storage(loc, calltrace, addr, storage_ty) in
       let* value = eval_ligo typed_exp calltrace env in
       return value
    | ( C_TEST_ORIGINATE , [ contract ; storage ; V_Ct ( C_mutez amt ) ] ) ->
       let contract_ty = List.nth_exn types 0 in
       let storage_ty = List.nth_exn types 1 in
       let>> code = Compile_contract (loc, contract, contract_ty) in
       let>> storage = Eval (loc, storage, storage_ty) in
       let>> size = Get_size code in
       let>> addr  = Inject_script (loc, calltrace, code, storage, amt) in
       return @@ V_Record (LMap.of_list [ (Label "0", addr) ; (Label "1", code) ; (Label "2", size) ])
    | ( C_TEST_EXTERNAL_CALL_TO_CONTRACT_EXN , [ (V_Ct (C_contract contract)) ; param ; V_Ct ( C_mutez amt ) ] ) ->
       let param_ty = List.nth_exn types 1 in
       let>> param = Eval (loc, param, param_ty) in
       (match param with
       | V_Michelson (Ty_code (param,_,_)) ->
          let>> err_opt = External_call (loc,calltrace,contract,param,amt) in
          (match err_opt with
                     | None -> return @@ V_Ct C_unit
                     | Some e -> fail @@ Errors.target_lang_error loc calltrace e)
       | _ -> fail @@ Errors.generic_error loc "Error typing param")
    | ( C_TEST_EXTERNAL_CALL_TO_CONTRACT , [ (V_Ct (C_contract contract)) ; param; V_Ct ( C_mutez amt ) ] ) ->
       let param_ty = List.nth_exn types 1 in
       let>> param = Eval (loc, param, param_ty) in
       (match param with
       | V_Michelson (Ty_code (param,_,_)) ->
          let>> err_opt = External_call (loc,calltrace,contract,param,amt) in
          (match err_opt with
           | None -> return (LC.v_ctor "Success" (LC.v_unit ()))
           | Some e ->
              let>> a = State_error_to_value e in
              return a)
       | _ -> fail @@ Errors.generic_error loc "Error typing param")
    | ( C_TEST_NTH_BOOTSTRAP_TYPED_ADDRESS , [ V_Ct (C_nat n) ] ) ->
      let n = Z.to_int n in
      let* parameter_ty', storage_ty' = monad_option (Errors.generic_error loc "Expected typed address") @@
                                          Ast_typed.get_t_typed_address expr_ty in
      let>> (address, parameter_ty, storage_ty) = Nth_bootstrap_typed_address (loc, n) in
      let* () = monad_option (Errors.generic_error loc "Parameter in bootstrap contract does not match") @@
                   Ast_typed.assert_type_expression_eq (parameter_ty, parameter_ty') in
      let* () = monad_option (Errors.generic_error loc "Storage in bootstrap contract does not match") @@
                   Ast_typed.assert_type_expression_eq (storage_ty, storage_ty') in
      return_ct (C_address address)
    | ( C_FAILWITH , [ a ] ) ->
      fail @@ Errors.meta_lang_failwith loc calltrace a
    | _ -> fail @@ Errors.generic_error loc "Unbound primitive."
  )

(*interpreter*)
and eval_literal : Ast_typed.literal -> value Monad.t = function
  | Literal_unit        -> Monad.return @@ V_Ct (C_unit)
  | Literal_int i       -> Monad.return @@ V_Ct (C_int i)
  | Literal_nat n       -> Monad.return @@ V_Ct (C_nat n)
  | Literal_timestamp i -> Monad.return @@ V_Ct (C_timestamp i)
  | Literal_string s    -> Monad.return @@ V_Ct (C_string (Ligo_string.extract s))
  | Literal_bytes s     -> Monad.return @@ V_Ct (C_bytes s)
  | Literal_mutez s     -> Monad.return @@ V_Ct (C_mutez s)
  | Literal_key_hash s  ->
     begin
       match Tezos_crypto.Signature.Public_key_hash.of_b58check s with
       | Ok kh -> Monad.return @@ V_Ct (C_key_hash kh)
       | Error _ -> Monad.fail @@ Errors.literal Location.generated (Literal_key_hash s)
     end
  | Literal_address s   ->
     begin
       match Tezos_protocol_008_PtEdo2Zk.Protocol.Alpha_context.Contract.of_b58check s with
       | Ok t -> Monad.return @@ V_Ct (C_address t)
       | Error _ -> Monad.fail @@ Errors.literal Location.generated (Literal_address s)
     end
  | l -> Monad.fail @@ Errors.literal Location.generated l

and eval_ligo : Ast_typed.expression -> calltrace -> env -> value Monad.t
  = fun term calltrace env ->
    let open Monad in
    match term.expression_content with
    | E_application {lamb = f; args} -> (
        let* f' = eval_ligo f calltrace env in
        let* args' = eval_ligo args calltrace env in
        match f' with
          | V_Func_val {arg_binder ; body ; env; rec_name = None ; orig_lambda } ->
            let in_ty, _ = Ast_typed.get_t_function_exn orig_lambda.type_expression in
            let f_env' = Env.extend ~ast_type:in_ty env (arg_binder, args') in
            eval_ligo body (term.location :: calltrace) f_env'
          | V_Func_val {arg_binder ; body ; env; rec_name = Some fun_name; _} ->
            let f_env' = Env.extend env (arg_binder, args') in
            let f_env'' = Env.extend f_env' (fun_name, f') in
            eval_ligo body (term.location :: calltrace) f_env''
          | _ -> fail @@ Errors.generic_error term.location "Trying to apply on something that is not a function?"
      )
    | E_lambda {binder; result;} ->
      return @@ V_Func_val {rec_name = None; orig_lambda = term ; arg_binder=binder ; body=result ; env}
    | E_let_in {let_binder ; rhs; let_result} -> (
      let* rhs' = eval_ligo rhs calltrace env in
      eval_ligo (let_result) calltrace (Env.extend ~ast_type:rhs.type_expression env (let_binder,rhs'))
    )
    | E_type_in {type_binder=_ ; rhs=_; let_result} -> (
      eval_ligo (let_result) calltrace env
    )
    | E_mod_in    _ -> fail @@
                         Errors.modules_not_supported term.location
    | E_mod_alias _ -> fail @@
                         Errors.modules_not_supported term.location
    | E_literal l ->
      eval_literal l
    | E_variable var ->
      let {eval_term=v} = try Option.value_exn (Env.lookup env var) with _ -> (failwith "unbound variable") in
      return v
    | E_record recmap ->
      let* lv' = Monad.bind_map_list
        (fun (label,(v:Ast_typed.expression)) ->
          let* v' = eval_ligo v calltrace env in
          return (label,v'))
        (LMap.to_kv_list_rev recmap)
      in
      return @@ V_Record (LMap.of_list lv')
    | E_record_accessor { record ; path} -> (
      let* record' = eval_ligo record calltrace env in
      match record' with
      | V_Record recmap ->
        let a = LMap.find path recmap in
        return a
      | _ -> failwith "trying to access a non-record"
    )
    | E_record_update {record ; path ; update} -> (
      let* record' = eval_ligo record calltrace env in
      match record' with
      | V_Record recmap ->
        if LMap.mem path recmap then
          let* field' = eval_ligo update calltrace env in
          return @@ V_Record (LMap.add path field' recmap)
        else
          failwith "field l does not exist in record"
      | _ -> failwith "this expression isn't a record"
    )
    | E_constant {cons_name ; arguments} -> (
      let* arguments' = Monad.bind_map_list
        (fun (ae:Ast_typed.expression) ->
          let* value = eval_ligo ae calltrace env in
          return @@ (value, ae.type_expression))
        arguments in
      apply_operator term.location calltrace term.type_expression env cons_name arguments'
    )
    | E_constructor { constructor = Label c ; element } when String.equal c "True"
      && element.expression_content = Ast_typed.e_unit () -> return @@ V_Ct (C_bool true)
    | E_constructor { constructor = Label c ; element } when String.equal c "False"
      && element.expression_content = Ast_typed.e_unit () -> return @@ V_Ct (C_bool false)
    | E_constructor { constructor = Label c ; element } ->
       let* v' = eval_ligo element calltrace env in
      return @@ V_Construct (c,v')
    | E_matching { matchee ; cases} -> (
      let* e' = eval_ligo matchee calltrace env in
      match cases, e' with
      | Match_variant {cases;_}, V_List [] ->
        let {constructor=_ ; pattern=_ ; body} =
          List.find_exn
            ~f:(fun {constructor = (Label c) ; pattern=_ ; body=_} ->
              String.equal "Nil" c)
            cases in
        eval_ligo body calltrace env
      | Match_variant {cases;tv}, V_List lst ->
        let {constructor=_ ; pattern ; body} =
          List.find_exn
            ~f:(fun {constructor = (Label c) ; pattern=_ ; body=_} ->
              String.equal "Cons" c)
            cases in
        let ty = Ast_typed.get_t_list_exn tv in
        let hd = List.hd_exn lst in
        let tl = V_List (List.tl_exn lst) in
        let proj = v_pair (hd,tl) in
        let env' = Env.extend ~ast_type:ty env (pattern, proj) in
        eval_ligo body calltrace env'
      | Match_variant {cases;_}, V_Ct (C_bool b) ->
        let ctor_body (case : matching_content_case) = (case.constructor, case.body) in
        let cases = LMap.of_list (List.map ~f:ctor_body cases) in
        let get_case c =
            (LMap.find (Label c) cases) in
        let match_true  = get_case "True" in
        let match_false = get_case "False" in
        if b then eval_ligo match_true calltrace env
        else eval_ligo match_false calltrace env
      | Match_variant {cases ; tv} , V_Construct (matched_c , proj) ->
        let* tv = match Ast_typed.get_t_sum tv with
          | Some tv ->
             let {associated_type} = LMap.find
                                  (Label matched_c) tv.content in
             return associated_type
          | None ->
             match Ast_typed.get_t_option tv with
             | Some tv -> return tv
             | None ->
                fail @@
                  (Errors.generic_error tv.location "Expected sum") in
        let {constructor=_ ; pattern ; body} =
          List.find_exn
            ~f:(fun {constructor = (Label c) ; pattern=_ ; body=_} ->
              String.equal matched_c c)
            cases in
        (* TODO-er: check *)
        let env' = Env.extend ~ast_type:tv env (pattern, proj) in
        eval_ligo body calltrace env'
      | Match_record {fields ; body ; tv = _} , V_Record rv ->
        let aux : label -> ( expression_variable * _ ) -> env -> env =
          fun l (v,ty) env ->
            let iv = match LMap.find_opt l rv with
              | Some x -> x
              | None -> failwith "label do not match"
            in
            Env.extend ~ast_type:ty env (v,iv)
        in
        let env' = LMap.fold aux fields env in
        eval_ligo body calltrace env'
      | _ , v -> failwith ("not yet supported case "^ Format.asprintf "%a" Ligo_interpreter.PP.pp_value v^ Format.asprintf "%a" Ast_typed.PP.expression term)
    )
    | E_recursive {fun_name; fun_type=_; lambda} ->
      return @@ V_Func_val { rec_name = Some fun_name ;
                             orig_lambda = term ;
                             arg_binder = lambda.binder ;
                             body = lambda.result ;
                             env = env }
    | E_raw_code {language ; code} -> (
      match code.expression_content with
      | E_literal (Literal_string x) ->
        let exp_as_string = Ligo_string.extract x in
        return @@ V_Ligo (language , exp_as_string)
      | _ -> failwith "impossible"
    )
    | E_module_accessor {module_name=_; element=_} ->
       fail @@
         Errors.modules_not_supported term.location



let try_eval expr env state r =
  Monad.eval (eval_ligo expr [] env) state r

let eval ~raise : Ast_typed.module_fully_typed -> env * Tezos_state.context =
  fun (Module_Fully_Typed prg) ->
    let aux : env * Tezos_state.context -> declaration location_wrap -> env * Tezos_state.context =
      fun (top_env,state) el ->
        match Location.unwrap el with
        | Ast_typed.Declaration_type _ ->
           (top_env,state)
        | Ast_typed.Declaration_constant {binder; expr ; inline=_ ; _} ->
          let (v,state) = try_eval ~raise expr top_env state None in
          let mich = match v with
            | V_Func_val _ | V_Michelson _ | V_Ligo _ ->
               None
            | _ ->
               let mich_expr,mich_expr_ty,_ = Michelson_backend.compile_simple_value ~raise ~loc:expr.location v expr.type_expression in
               Some (mich_expr, mich_expr_ty) in
          let top_env' = Env.extend ~ast_type:expr.type_expression ?micheline:mich top_env (binder, v) in
          (top_env',state)
        | Ast_typed.Declaration_module {module_binder; module_=_} ->
          let module_env =
            raise.raise @@
              Errors.modules_not_supported el.location
          in
          let top_env' = Env.extend top_env (Location.wrap @@ Var.of_name module_binder, module_env) in
          (top_env',state)
        | Ast_typed.Module_alias _ ->
           raise.raise @@
             Errors.modules_not_supported el.location
    in
    let initial_state = Tezos_state.init_ctxt ~raise [] in
    let (env,state) = List.fold ~f:aux ~init:(Env.empty_env, initial_state) prg in
    (env, state)

let eval_test ~raise : Ast_typed.module_fully_typed -> (string * value) list =
  fun prg ->
    let (env, _state) = eval ~raise prg in
    let v = Env.to_kv_list_rev env in
    let aux : expression_variable * value_expr -> (string * value) option = fun (ev, v) ->
      let ev = Location.unwrap ev in
      if not (Var.is_generated ev) && (Base.String.is_prefix (Var.to_name ev) ~prefix:"test") then
        Some (Var.to_name ev, v.eval_term)
      else
        None
    in
    List.filter_map ~f:aux v

let () = Printexc.record_backtrace true
