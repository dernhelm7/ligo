(*
Currently tezos primitive OPEN_CHEST return type in LIGO is:
```
| Ok_opening of bytes
| Fail_decrypt
| Fail_timelock
```
and give (compiled to michelson):
```
or (or (unit %fail_decrypt) (unit %fail_timelock)) (bytes %ok_opening)
```

This test makes sure all of the cases above can be constructed using michelson insertion
*)

let create_ok_open (b:bytes) : chest_opening_result =
  [%Michelson ({| { RIGHT (or (unit) (unit)) } |} : bytes -> chest_opening_result) ] b

let create_fail_d (b:unit) : chest_opening_result =
  [%Michelson ({| { LEFT unit ; LEFT bytes } |} : unit -> chest_opening_result) ] b

let create_fail_t (b:unit) : chest_opening_result =
  [%Michelson ({| { RIGHT unit ; LEFT bytes } |} : unit -> chest_opening_result) ] b

type storage = chest_opening_result
type return = operation list * storage
type parameter = | Ok_o of bytes | Fail_d | Fail_t

let main ((p,_):parameter * storage) : return =
  let s =
    match p with
    | Ok_o b -> create_ok_open b 
    | Fail_d -> create_fail_d ()
    | Fail_t -> create_fail_t ()
  in
  (([] : operation list), s)