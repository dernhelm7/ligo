---
id: getting started
title: Getting started
---

This section is aimed at newcomers to Ligo and Tezos smart-contracts.
In this turorial, we will go through the following step :
-	Setting up the development environment,
-	Writting a simple contract
-	Testing the contract
-	Deploying the contract to tezos

## Setting up the development environment.
At the present moment, we recommand the user the develop on a UNIX system, GNU/Linux or MacOSX as the windows native binary is still in preparation. You can still use Ligo on windows.

More on [installation](https://ligolang.org/docs/intro/installation) and [editor support](https://ligolang.org/docs/intro/editor-support)

Alternatively, you can decide to use our [webide](https://ide.ligolang.org/). This can be usefull for testing or small project. But you won't be able to save or do multiple file project
### Install ligo

If you are on linux, we have a `.deb` package ready for you. Those package are widely supported by linux distribution

* On debian or unbuntun, download [the package](https://ligolang.org/deb/ligo.deb), and then install using: 

```zsh
sudo apt install ./ligo.deb
```

* On Archlinux and Manjaro, there is an AUR package availiable. (more information on the [AUR](https://wiki.archlinux.org/title/Arch_User_Repository))

* On MacOsX, a [brew](https://brew.sh/) formula is availiable and can be download with `brew install ligo`.

* If you are using another distribution, refer to their doc on how to install `.deb` packages.

* Alternatively, you can download the program and install it by hand by running
```zsh
wget https://ligolang.org/bin/linux/ligo
chmod +x ./ligo
```
Move it to you path for global install
```zsh
sudo cp ./ligo /usr/local/bin
```
If you choose this method, you will have to manually update the program by reproducing those step.

Check that the installation is correct by opening a terminal and running the command
```zsh
ligo --version
```
If you get an error message, start again.
If you don't, then let setup our editor.
### Setting up the editor 

You can see the updated list of supported editor [here](https://ligolang.org/docs/intro/editor-support)


In this tutorial, we will use vs-code.

* For vs-code, simply go to the extension menu in the left bar (Ctrl + Shift + X) and search for the `ligo-vscode` extension and install it.

* For emacs, follow the instruction [here](https://gitlab.com/ligolang/ligo/-/blob/dev/tools/emacs/README.md)

* For vim, follow the instruction [here](https://gitlab.com/ligolang/ligo/-/blob/dev/tools/vim/ligo/start/ligo/README.md)

Once, you've done it, you are ready to make your first smart-contract

### Install the tezos tools

For deploying your smart-contract on the network and for some testing, you will need to use a tezos client.

* On GNU/linux, the simplest way to get tezos-client is through opam using `opam install tezos-client`. alternatives are avaliable [here](https://tezos.gitlab.io/introduction/howtoget.html)

* On MacOsX, the sowtfare is distributed through a brew formula with `brew install tezos`.

## Building a smart-contract.

In this section and the following one we will use a simple smart-contract that is present as example on our webide. We will cover the ligo language and smart-contract development in the following tutorials.

First, create a `ligo_tutorial` folder on your computer. Then download and put the contract in this folder. It is availiable in [Pascaligo](),[Cameligo]() and [Reasonligo](). In the following, we consider that you are using the Cameligo contract, simply change the extension (`.mligo` for cameligo, `.ligo` for pascaligo, `.religo` for reasonligo) in case you use another one.

Open your editor in the folder and the file in the editor. you should have this code
```ocaml
type storage = int

type parameter =
  Increment of int
| Decrement of int
| Reset

type return = operation list * storage

// Two entrypoints

let add (store, delta : storage * int) : storage = store + delta
let sub (store, delta : storage * int) : storage = store - delta

(* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. *)
   
let main (action, store : parameter * storage) : return =
 ([] : operation list),    // No operations
 (match action with
   Increment (n) -> add (store, n)
 | Decrement (n) -> sub (store, n)
 | Reset         -> 0)
```

Now we gonna compile the contract, open a terminal in the folder. (or the vs-code built-in terminal with  Ctrl+shift+²) and run the following command:

```zsh
ligo compile-contract increment.mligo main --output=increment.tz
```

The compile-contract take two parameter, the file you want to compile and the function that will be use as entry point, the --output parameter indicate to store the result in increment.tz instead of outputting it in the terminal.

Now, you should have a michelson contract `increment.tz` in the folder ready to be deploy. But before that, we want to test it to be sure that it behaves as expected, because once publish, it cannot be modified.

## Testing the contract

As the can never underline enough the importance of test in the context of smart-contract. We will now test our contract three time on different level :

### Test the code from the command line

Using the `interpret` command, one can run ligo code in the context of an init file. For intance

```zsh
ligo interpret --init-file increment.mligo "<code>" will
``` 

will run `<code>` after evaluating everything in increment.mligo. This is usefull to test arbitrary function and variable in your code.

For intance, to test the add function you can run
```zsh
ligo interpret --init-file increment.mligo "add(10,32)"
```
which should return 42.
Running several of this command will cover the complete code.

To run the contract as called on the blockchain, you will prefer the command `dry-run` which take the contract, the entrypoint, the initial parameter and the initial storage, like so
```zsh
ligo dry-run increment.mligo main "Increment(32)" "10"
```
which will will return (LIST_EMPTY(), 42).

Combine several of those command to fully test the contract use-cases.


### Test the code with ligo test framework.

In ligo, you are able to write test directly in the source file, using the test module. 

Add the folowing line at the end of `increment.mligo`

```ocaml
let _test () =
  let initial_storage = 10 in
  let (taddr, _, _) = Test.originate main  initial_storage 0tez in
  let contr = Test.to_contract(taddr) in
  let _r = Test.transfer_to_contract_exn contr (Increment (32)) 1tez  in
  (Test.get_storage(taddr) = initial_storage + 32)

let test = _test ()
```

which execute the same test as the previous section.

Now simply run the command
```zsh
ligo test increment.mligo
```

The command will run every function starting with `test`

more on the syntax for the test framework [here](https://ligolang.org/docs/advanced/testing#testing-with-test)


### Testing the michelson contract

The ligo compiler is made so the produced michelson program types and correspond to the initial ligo program. However untill we have the tool for formal verification (which is ongoing work), you shouldn't trust that the michelson code will behave as the ligo one and also write test for the michelson code.

There is different methods for testing michelson code. In this tutorial we will focus on tezos-client mockup. More information [here](https://ligolang.org/docs/advanced/michelson_testing)

This method consist in running a "mockup" tezos chain on our computer, push the contract on the chain and send transaction to the chain to test the contract behavior.

Fist, create a tmp folder for the mockup chain by runnig 
```zsh
mkdir /tmp/mockup
```

Now start the node by running
```zsh
tezos-client \
  --protocol PtEdoTezd3RHSC31mpxxo1npxFjoWWcFgQtxapi51Z8TLu6v6Uq \
  --base-dir /tmp/mockup \
  --mode mockup \
  create mockup
```

This will run the node using the `Edo` protocol and return a few address, aliased from bootstrap1 to 5. For other version, check 
`tezos-client list mockup protocols`

You can now originate the contract to the mock net with :
```zsh
tezos-client \                                                      
  --protocol PtEdo2ZkT9oKpimTah6x2embF25oss54njMuPzkJTEi5RqfdZFA \
  --base-dir /tmp/mockup \
  --mode mockup \
  originate contract mockup_testme \
              transferring 0 from bootstrap1 \
              running increment.tz \
              --init 10 --burn-cap 0.1
```
you should see a lot of information on the command line and the information `New contract ... origninated`

You can now start testing the contract.

To check its storage run :
```zsh
tezos-client \                                                      
  --protocol PtEdo2ZkT9oKpimTah6x2embF25oss54njMuPzkJTEi5RqfdZFA \
  --base-dir /tmp/mockup \
  --mode mockup \
  get contract storage for mockup_testme
```
You should see a `10` in your terminal

We are now ready to send a transaction to our contract. We want to send a transaction with parameter "Increment (32)" but the parameter is written is ligo.
For that, it must first be converted to a michelson parameter. Which is done by running :

```zsh
ligo compile-parameter increment.mligo main "Increment (32)"
```

Which give you the result (Left (Right 32))

Now we can send our transaction with the command

```zsh
tezos-client \
  --protocol PtEdo2ZkT9oKpimTah6x2embF25oss54njMuPzkJTEi5RqfdZFA \
  --base-dir /tmp/mockup \
  --mode mockup \
transfer 0 from bootstrap2 \
              to mockup_testme \
              --arg "(Left (Right 32))" --burn-cap 0.01
```
The network will again send back many information including the updated storage which should now be equal to 42.

This conclude our section about testing. As a exercice, you can write the test for the other entrypoint (decrease and reset).
Once you are sure that the contract work corectly for all the use cases, you can move on to the next section

## Publishing the contract

For deploying the contract on tezos, we will use the `tezos-client` interface like we did on the previous section.

First, you will need an account address. You can get one for testing at the [faucet](https://faucet.tzalpha.net/)
download the json file and place it in the `ligo_tutorial` folder

Then we gonna point the clien on a tezos node
```zsh
tezos-client --endpoint https://edo-tezos.giganode.io config update 
```

Once done, activate your account
```zsh
tezos-client activate account alice with <the name of the json file>
```

You are now ready to originate your contract
```zsh
tezos-client originate contract increment \
              transferring 0 from alice \
              running increment.tz \
              --init 10 --burn-cap 0.1
```

You can search your contract on the network using the portal [Better call dev](https://better-call.dev/)
