type action =
| Increment of int
| Decrement of int

let add (a : int) (b : int) : int = a + b

let subtract (a : int) (b : int) : int = a - b

(* real entrypoint that re-routes the flow based on the action provided *)

let%entry main (p : action) storage =
  let storage =
    match p with
    | Increment n -> add s n
    | Decrement n -> subtract s n
  in ([] : operation list), storage
