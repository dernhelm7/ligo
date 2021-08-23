let map_incr_op (s : int set) : int set = 
    Set.map (fun (x : int) -> x + 1) s
(*
let map_conv_op (s : nat set) : int set =
    Set.map (fun (x : nat) -> int (x)) s

let sum (x, y : int * int) : int = x + y

let fold_sum_op (s : int set) : int =
    Set.fold sum s 0

let string_concat (x, y : string * string) : string = x ^ y

let fold_string_concat_op (s : string set) : string =
    Set.fold string_concat s ""

let fold_desc_sum_op (s : int set) : int =
    Set.fold_desc sum s 0

let fold_desc_string_concat_op (s : string set) : string =
    Set.fold_desc string_concat s ""

(* set.iter?? *)

*)