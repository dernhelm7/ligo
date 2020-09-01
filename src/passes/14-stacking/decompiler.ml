open Errors
open Mini_c.Types
open Proto_alpha_utils.Memory_proto_alpha
open X
open Proto_alpha_utils.Trace
open Protocol
open Script_typed_ir

let rec decompile_value (Ex_typed_value (ty, value)) : (value , stacking_error) result =
  match (ty, value) with
  | Pair_t ((a_ty, _, _), (b_ty, _, _), _ , _), (a, b) -> (
      let%bind a = decompile_value @@ Ex_typed_value(a_ty, a) in
      let%bind b = decompile_value @@ Ex_typed_value(b_ty, b) in
      ok @@ D_pair(a, b)
    )
  | Union_t ((a_ty, _), _, _ , _), L a -> (
      let%bind a = decompile_value @@ Ex_typed_value(a_ty, a) in
      ok @@ D_left a
    )
  | Union_t (_, (b_ty, _), _ , _), R b -> (
      let%bind b = decompile_value @@ Ex_typed_value(b_ty, b) in
      ok @@ D_right b
    )
  | (Int_t _), n ->
      let n = Alpha_context.Script_int.to_zint n in
      ok @@ D_int n
  | (Nat_t _), n ->
      let n = Alpha_context.Script_int.to_zint n in
      ok @@ D_nat n
  | (Chain_id_t _), id ->
    let str = Tezos_crypto.Base58.simple_encode
      (Tezos_base__TzPervasives.Chain_id.b58check_encoding)
      id in
    ok @@ D_string str
  | (Key_hash_t _ ), n ->
    ok @@ D_string (Signature.Public_key_hash.to_b58check n)
  | (Key_t _ ), n ->
    ok @@ D_string (Signature.Public_key.to_b58check n)
  | (Signature_t _ ), n ->
    ok @@ D_string (Signature.to_b58check n)
  | (Timestamp_t _), n ->
      let n = Alpha_context.Script_timestamp.to_zint n in
      ok @@ D_timestamp n
  | (Mutez_t _), n ->
      let%bind n =
        generic_try (corner_case ~loc:__LOC__ "too big to fit an int") @@
        (fun () -> Z.of_int64 @@ Alpha_context.Tez.to_mutez n) in
      ok @@ D_mutez n
  | (Bool_t _), b ->
      ok @@ D_bool b
  | (String_t _), s ->
      ok @@ D_string s
  | (Bytes_t _), b ->
      ok @@ D_bytes b
  | (Address_t _), (s , _) ->
      ok @@ D_string (Alpha_context.Contract.to_b58check s)
  | (Unit_t _), () ->
      ok @@ D_unit
  | (Option_t _), None ->
      ok @@ D_none
  | (Option_t (o_ty, _, _)), Some s ->
      let%bind s' = decompile_value @@ Ex_typed_value (o_ty, s) in
      ok @@ D_some s'
  | (Map_t (k_cty, v_ty, _ , _)), m ->
      let k_ty = X.ty_of_comparable_ty k_cty in
      let lst =
        let aux k v acc = (k, v) :: acc in
        let lst = Script_ir_translator.map_fold aux m [] in
        List.rev lst in
      let%bind lst' =
        let aux (k, v) =
          let%bind k' = decompile_value (Ex_typed_value (k_ty, k)) in
          let%bind v' = decompile_value (Ex_typed_value (v_ty, v)) in
          ok (k', v')
        in
        bind_map_list aux lst
      in
      ok @@ D_map lst'
  | (Big_map_t (k_cty, v_ty, _)), m ->
      let k_ty = X.ty_of_comparable_ty k_cty in
      let lst =
        let aux k v acc = (k, v) :: acc in
        let lst = Script_ir_translator.map_fold aux m.diff [] in
        List.rev lst in
      let%bind lst' =
        let aux orig (k, v) =
          let%bind k' = decompile_value (Ex_typed_value (k_ty, k)) in
          let orig_rem = List.remove_assoc k' orig in
          match v with
          | Some vadd ->
            let%bind v' = decompile_value (Ex_typed_value (v_ty, vadd)) in
            if (List.mem_assoc k' orig) then ok @@ (k', v')::orig_rem
            else ok @@ (k', v')::orig
          | None -> ok orig_rem in
        bind_fold_list aux [] lst in
      ok @@ D_big_map lst'
  | (List_t (ty, _ , _)), lst ->
      let%bind lst' =
        let aux = fun t -> decompile_value (Ex_typed_value (ty, t)) in
        bind_map_list aux lst
      in
      ok @@ D_list lst'
  | (Set_t (ty, _)), (module S) -> (
      let lst = S.OPS.elements S.boxed in
      let lst' =
        let aux acc cur = cur :: acc in
        let lst = List.fold_left aux lst [] in
        List.rev lst in
      let%bind lst'' =
        let aux = fun t -> decompile_value (Ex_typed_value (ty_of_comparable_ty ty, t)) in
        bind_map_list aux lst'
      in
      ok @@ D_set lst''
    )
  | (Operation_t _) , (op , _) ->
      let op =
        Data_encoding.Binary.to_bytes_exn
          Alpha_context.Operation.internal_operation_encoding
          op in
      ok @@ D_operation op
  | (Lambda_t _ as ty) , _ ->
      let%bind m_ty = trace_strong (corner_case ~loc:"TODO" "TODO") @@
        trace_tzresult_lwt unrecognized_data @@
        Proto_alpha_utils.Memory_proto_alpha.unparse_michelson_ty ty in
      let pp_lambda =
        Format.asprintf "[lambda of type: %a ]" Michelson.pp m_ty in
        ok @@ D_string pp_lambda
  | ty, v ->
      let%bind error = trace_strong (corner_case ~loc:"TODO" "TODO") @@
        let%bind m_data =
          trace_tzresult_lwt unrecognized_data @@
          Proto_alpha_utils.Memory_proto_alpha.unparse_michelson_data ty v in
        let%bind m_ty =
          trace_tzresult_lwt unrecognized_data @@
          Proto_alpha_utils.Memory_proto_alpha.unparse_michelson_ty ty in
        fail (untranspilable m_data m_ty)
      in
      fail error
