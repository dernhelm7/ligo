open Trace
open Tezos_utils
open Michelson
open Tezos_micheline.Micheline

type 'error mapper = michelson -> (michelson,'error) result

let rec map_expression : 'error mapper -> michelson -> (michelson,_) result = fun f e ->
  let self = map_expression f in
  let%bind e' = f e in
  match e' with
  | Prim (l , p , lst , a) -> (
      let%bind lst' = bind_map_list self lst in
      ok @@ Prim (l , p , lst' , a)
    )
  | Seq (l , lst) -> (
      let%bind lst' = bind_map_list self lst in
      ok @@ Seq (l , lst')
    )
  | x -> ok x

let fetch_contract_inputs : michelson -> (michelson * michelson) option =
  function
  | Prim (_, "lambda", [Prim (_, "pair", [param_ty; storage_ty], _); _], _) ->
    Some (param_ty, storage_ty)
  | _ -> None
