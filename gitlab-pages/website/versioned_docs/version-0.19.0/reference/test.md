---
id: test
title: Test
description: Test operations
hide_table_of_contents: true
---


import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

> Important: The `Test` module is only available inside the `ligo test` command. See also [Testing LIGO](../advanced/testing).

<SyntaxTitle syntax="pascaligo">
type michelson_program
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
type michelson_program
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
type michelson_program
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
type michelson_program
</SyntaxTitle>
A type for code that's compiled to Michelson.

<SyntaxTitle syntax="pascaligo">
type ligo_program
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
type ligo_program
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
type ligo_program
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
type ligo_program
</SyntaxTitle>
A type for code insertions in the test file.

<SyntaxTitle syntax="pascaligo">
type test_exec_error = 
  Rejected of (michelson_program * address) 
| Other
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
type test_exec_error = 
  Rejected of (michelson_program * address) 
| Other
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
type test_exec_error = 
  Rejected(michelson_program, address) 
| Other
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
type test_exec_error = 
  ["Rejected", michelson_program, address] 
| ["Other"]
</SyntaxTitle>
A test error.

<SyntaxTitle syntax="pascaligo">
type test_exec_result = 
  Success
| Fail of test_exec_error
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
type test_exec_result = 
  Success
| Fail of test_exec_error
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
type test_exec_result = 
  Success
| Fail(test_exec_error)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
type test_exec_result =
  ["Success"] 
| ["Fail", test_exec_error]
</SyntaxTitle>
A test execution result.

<SyntaxTitle syntax="pascaligo">
type typed_address ('p, 's)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
type ('p, 's) typed_address
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
type typed_address ('p, 's)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
type typed_address &lt;&apos;p, &apos;s&gt;
</SyntaxTitle>
A type for an address of a contract with parameter `'p` and storage
`'s`.

<SyntaxTitle syntax="pascaligo">
function to_contract : typed_address ('p, 's) -> contract ('p)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val to_contract : ('p, 's) typed_address -> 'p contract
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let to_contract : (typed_address ('p, 's)) => contract ('p)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let to_contract = (account: typed_address &lt;&apos;p, &apos;s&gt;) => contract &lt;&apos;p&gt;
</SyntaxTitle>

Get the contract corresponding to the default entrypoint of a typed
address: the contract parameter in the result will be the type of the
default entrypoint (generally `'p`, but this might differ if `'p`
includes a "default" entrypoint).

<SyntaxTitle syntax="pascaligo">
function to_entrypoint : string -> typed_address ('p, 's) -> contract ('e)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val to_entrypoint : string -> ('p, 's) typed_address -> 'e contract
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let to_entrypoint : string => (typed_address ('p, 's)) => contract ('e)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let to_entrypoint = (entrypoint: string, account: typed_address &lt;&apos;p, &apos;s&gt;) => contract &lt;&apos;e&gt;
</SyntaxTitle>

Get the contract corresponding to an entrypoint of a typed address:
the contract parameter in the result will be the type of the
entrypoint, it needs to be annotated, entrypoint string should omit
the prefix "%".

<SyntaxTitle syntax="pascaligo">
function originate_from_file : string -> string -> michelson_program -> tez -> (address * michelson_program * int)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val originate_from_file : string -> string -> michelson_program -> tez -> (address * michelson_program * int)
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let originate_from_file : string => string => michelson_program => tez => (address, michelson_program, int)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let originate_from_file = (filepath: string, entrypoint: string, init: michelson_program, balance: tez) => [address, michelson_program, int]
</SyntaxTitle>

Originate a contract with an entrypoint, initial storage and initial balance.

<Syntax syntax="pascaligo">

```pascaligo skip
const originated = Test.originate_from_file(testme_test, "main", init_storage, 0tez);
const addr = originated.0;
const program = originated.1;
const size = originated.2;
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo skip
let (addr, program, size) = Test.originate_from_file testme_test "main" init_storage 0tez in
...
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo skip
let (addr, program, size) = Test.originate_from_file(testme_test,"main", init_storage, 0tez);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo skip
let [addr, program, size] = Test.originate_from_file(testme_test,"main", init_storage, 0 as tez);
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
function originate : ('parameter * 'storage -> list (operation) * 'storage) -> 'storage -> tez -> (typed_address ('parameter, 'storage) * michelson_program * int)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val originate : ('parameter * 'storage -> operation list * 'storage) -> 'storage -> tez -> (('parameter, 'storage) typed_address * michelson_program * int)
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let originate : (('parameter, 'storage) -> (list(operation), 'storage)) => 'storage => tez => (typed_address ('parameter, 'storage), michelson_program, int)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let originate = (contract: ('parameter, 'storage) => (list &lt;operation&gt;, &apos;storage), init: 'storage, balance: tez) => [typed_address &lt;&apos;parameter, &apos;storage&gt;, michelson_program, int]
</SyntaxTitle>

Originate a contract with an entrypoint function, initial storage and initial balance.

<SyntaxTitle syntax="pascaligo">
function set_now : timestamp -> unit
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val set_now : string -> unit
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let set_now: timestamp => unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let set_now = (now: timestamp) => unit
</SyntaxTitle>
Set the timestamp of the predecessor block.

<SyntaxTitle syntax="pascaligo">
function set_source : address -> unit
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val set_source : address -> unit
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let set_source: address => unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let set_source = (source: address) => unit
</SyntaxTitle>
Set the source for `Test.transfer` and `Test.originate`.

<SyntaxTitle syntax="pascaligo">
function set_baker : address -> unit
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val set_baker : address -> unit
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let set_baker: address => unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let set_baker = (source: address) => unit
</SyntaxTitle>
Force the baker for `Test.transfer` and `Test.originate`. By default, the first bootstrapped account.

<SyntaxTitle syntax="pascaligo">
function transfer : address -> michelson_program -> tez -> test_exec_result
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val transfer : address -> michelson_program -> tez -> test_exec_result
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let transfer: (address, michelson_program, tez) => test_exec_result
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let transfer = (addr: address, parameter: michelson_program, amount: tez) => test_exec_result
</SyntaxTitle>
Bake a transaction by sending an amount of tez with a parameter from the current source to another account.

<SyntaxTitle syntax="pascaligo">
function transfer_exn : address -> michelson_program -> tez -> unit
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val transfer_exn : address -> michelson_program -> tez -> unit
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let transfer_exn: (address, michelson_program, tez) => unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let transfer_exn = (addr: address, parameter: michelson_program, amount: tez) => unit
</SyntaxTitle>
Similar as `Test.transfer`, but fails when anything goes wrong.

<SyntaxTitle syntax="pascaligo">
function transfer_to_contract : contract ('p) -> 'p -> tez -> test_exec_result
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val transfer_to_contract : 'p contract -> 'p -> tez -> test_exec_result
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let transfer_to_contract: (contract ('p), 'p, tez) => test_exec_result
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let transfer_to_contract = (addr: contract&lt;&apos;p&gt;, parameter: &apos;p, amount: tez) => test_exec_result
</SyntaxTitle>
Bake a transaction by sending an amount of tez with a parameter from the current source to a contract.

<SyntaxTitle syntax="pascaligo">
function transfer_to_contract_exn : contract ('p) -> 'p -> tez -> unit
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val transfer_to_contract_exn : 'p contract -> 'p -> tez -> unit
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let transfer_to_contract_exn: (contract ('p), 'p, tez) => unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let transfer_to_contract_exn = (addr: contract&lt;&apos;p&gt;, parameter: &apos;p, amount: tez) => unit
</SyntaxTitle>
Similar as `Test.transfer_to_contract`, but fails when anything goes wrong.

<SyntaxTitle syntax="pascaligo">
function get_storage_of_address : address -> michelson_program
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val get_storage_of_address : address -> michelson_program
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let get_storage_of_address : (address) => michelson_program
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get_storage_of_address = (account: address) => michelson_program
</SyntaxTitle>
Get the storage of an account in `michelson_program`.

<SyntaxTitle syntax="pascaligo">
function get_storage : typed_address ('p, 's) -> 's
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val get_storage : ('p, 's) typed_address -> 's
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let get_storage: (typed_address ('p, 's)) => 's
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get_storage = (account: typed_address &lt;&apos;p, &apos;s&gt;) => &apos;s
</SyntaxTitle>
Get the storage of a typed account.

<SyntaxTitle syntax="pascaligo">
function get_balance : address -> tez
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val get_balance : address -> tez
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let get_balance: (address) => tez
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get_balance = (account: address) => tez
</SyntaxTitle>
Get the balance of an account in tez.

<SyntaxTitle syntax="pascaligo">
function michelson_equal : michelson_program -> michelson_program -> bool
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val michelson_equal : michelson_program -> michelson_program -> bool
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let michelson_equal: (michelson_program, michelson_program) => bool
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let michelson_equal = (a: michelson_program, b: michelson_program) => bool
</SyntaxTitle>
Compare two Michelson values.

<SyntaxTitle syntax="pascaligo">
function log : 'a -> unit
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val log : 'a -> unit
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let log: 'a => unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let log = (a: 'a) => unit
</SyntaxTitle>
Log a value.

<SyntaxTitle syntax="pascaligo">
function reset_state : nat -> list(nat) -> unit
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val reset_state : nat -> nat list -> unit
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let reset_state: (nat, list(nat)) => unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let reset_state = (no_of_accounts: nat, amount: list&lt;nat&gt;) => unit
</SyntaxTitle>
Generate a number of random bootstrapped accounts with a default amount of 4000000 tez. The passed list can be used to overwrite the amount.
By default, the state only has two bootstrapped accounts.

<SyntaxTitle syntax="pascaligo">
function nth_bootstrap_account : int -> address
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val nth_bootstrap_account : int -> address
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let nth_bootstrap_account: int => address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let nth_bootstrap_account = (nth: int) => address
</SyntaxTitle>
Returns the address of the nth bootstrapped account.

<SyntaxTitle syntax="pascaligo">
function last_originations : unit -> map (address * list(address))
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val last_originations : unit -> (address * address list) map
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let last_originations: unit => map (address , list(address))
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let last_originations = (_: unit) => map&lt;address , address list&gt;
</SyntaxTitle>
Returns the address of the nth bootstrapped account.


<SyntaxTitle syntax="pascaligo">
function compile_expression : option(string) -> ligo_program -> michelson_program
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val compile_expression : string option -> ligo_program -> michelson_program
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let compile_expression: (option(string), ligo_program) => michelson_program
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let compile_expression = (filepath: option&lt;string&gt;, code: ligo_program) => michelson_program
</SyntaxTitle>
Compile an expression to Michelson and evaluate it.

<Syntax syntax="pascaligo">

```pascaligo skip
const expr = Test.compile_expression(Some("filename"), [%pascaligo ({| (42: int) |} : ligo_program)]);
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo skip
let expr = Test.compile_expression (Some("filename")) ([%cameligo ({| (42: int) |} : ligo_program)]) in
...
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo skip
let expr = Test.compile_expression(Some("filename"), ([%reasonligo ({| (42: int) |} : ligo_program)]));
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo skip
let expr = Test.compile_expression(Some("filename"), jsligo`42 as int` as ligo_program);
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
function compile_expression_subst : option(string) -> ligo_program -> list (string * michelson_program) -> michelson_program
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val compile_expression_subst : string option -> ligo_program -> (string * michelson_program) list -> michelson_program
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let compile_expression_subst: (option(string), ligo_program, list((string, michelson_program))) => michelson_program
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let compile_expression_subst = (filepath: option&lt;string&gt;, code: ligo_program, subst_list: list&lt;[string, michelson_program]&gt;]) => michelson_program
</SyntaxTitle>

<Syntax syntax="pascaligo">

```pascaligo skip
const example = Test.compile_expression_subst (Some(under_test),
    [%pascaligo ({| {one = $one ; two = $two ; three = $three ; four = $four ; five = $five} |} : ligo_program)],
    list [
      ("one", d_one );
      ("two", Test.compile_value (1n + 2n)) ;
      ("three", Test.compile_value ("a"^"b")) ;
      ("four", Test.compile_value (0xFF00)) ;
      ("five", Test.compile_value ()) ;
    ]);
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo skip
let example = Test.compile_expression_subst (Some under_test)
    [%cameligo ({| {one = $one ; two = $two ; three = $three ; four = $four ; five = $five} |} : ligo_program)]
    [
      ("one", d_one );
      ("two", Test.compile_value (1n + 2n)) ;
      ("three", Test.compile_value ("a"^"b")) ;
      ("four", Test.compile_value (0xFF00)) ;
      ("five", Test.compile_value ()) ;
    ]
  in
  ...
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo skip
let example = Test.compile_expression_subst (Some (under_test),
    [%reasonligo ({| {one: $one, two: $two, three: $three, four: $four, five: $five} |} : ligo_program)],
    [
      ("one", d_one ),
      ("two", Test.compile_value (1n + 2n)) ,
      ("three", Test.compile_value ("a"^"b")) ,
      ("four", Test.compile_value (0xFF00)) ,
      ("five", Test.compile_value ())
    ]); 
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo skip
let example = Test.compile_expression_subst (Some (under_test),
    jsligo`{one: $one, two: $two, three: $three, four: $four, five: $five}` as ligo_program,
    list([
      ["one", d_one],
      ["two", Test.compile_value ((1 as nat) + (2 as nat))] ,
      ["three", Test.compile_value ("a" + "b")] ,
      ["four", Test.compile_value (0xFF00)],
      ["five", Test.compile_value ()]
    ])); 
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
function compile_value : 'a -> michelson_program
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val compile_value : 'a -> michelson_program
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let compile_value: 'a => michelson_program
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let compile_value = (value: 'a) => michelson_program
</SyntaxTitle>
Compile a LIGO value to Michelson.

<SyntaxTitle syntax="pascaligo">
function eval : 'a -> michelson_program
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val eval : 'a -> michelson_program
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let eval: 'a => michelson_program
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let eval = (value: 'a) => michelson_program
</SyntaxTitle>
Compile a LIGO value to Michelson. Currently it is a renaming of
`compile_value`.

<SyntaxTitle syntax="pascaligo">
function run : ('a -> 'b) -> 'a -> michelson_program
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val run : ('a -> 'b) -> 'a -> michelson_program
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let run : ('a => 'b) => 'a => michelson_program
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let run = (func: ('a => 'b), value: 'a) => michelson_program
</SyntaxTitle>

Run a function on an input, all in Michelson. More concretely:
a) compiles the function argument to Michelson `f_mich`;
b) compiles the value argument (which was evaluated already) to Michelson `v_mich`;
c) runs the Michelson interpreter on the code `f_mich` with starting stack `[ v_mich ]`.
