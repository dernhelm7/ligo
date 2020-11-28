(* This tests that we subvert the Micheline whitespace check *)
let f = [%Michelson ({|
{ DROP;
UNIT
} |} : unit -> unit)]

let main (p, s : unit * unit) : operation list * unit =
  (([] : operation list), f s)
