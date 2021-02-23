open Cli_expect

let%expect_test _ =
  run_ligo_good [ "transpile-contract" ; "../../test/contracts/coase.ligo" ; "pascaligo" ] ;
  [%expect {|
    type card_pattern_id is nat

    type card_pattern is
      record [coefficient : tez; quantity : nat]

    type card_patterns is map (card_pattern_id, card_pattern)

    type card_id is nat

    type card is
      record [
        card_owner : address;
        card_pattern : card_pattern_id
      ]

    type cards is map (card_id, card)

    type storage is
      record [
        card_patterns : card_patterns;
        cards : cards;
        next_id : nat
      ]

    type return is list (operation) * storage

    type action_buy_single is
      record [card_to_buy : card_pattern_id]

    type action_sell_single is record [card_to_sell : card_id]

    type action_transfer_single is
      record [card_to_transfer : card_id; destination : address]

    type parameter is
        Buy_single of action_buy_single
      | Sell_single of action_sell_single
      | Transfer_single of action_transfer_single

    function transfer_single
      (const gen__parameters1 : action_transfer_single * storage) is
      case gen__parameters1 of [
        (action, s) ->
          block {
            const cards : cards = s.cards;
            const card : card
            = case cards [action.card_to_transfer] of [
                Some (card) -> card
              | None ->
                  (failwith ("transfer_single: No card.")
                   : card)
              ];
            if NEQ (card.card_owner, Tezos.sender)
            then failwith ("This card doesn't belong to you")
            else skip;
            card.card_owner := action.destination;
            cards [action.card_to_transfer] := card;
            s.cards := cards
          } with ((list [] : list (operation)), s)
      ]

    function sell_single
      (const gen__parameters2 : action_sell_single * storage) is
      case gen__parameters2 of [
        (action, s) ->
          block {
            const card : card
            = case s.cards [action.card_to_sell] of [
                Some (card) -> card
              | None ->
                  (failwith ("sell_single: No card.") : card)
              ];
            if NEQ (card.card_owner, Tezos.sender)
            then failwith ("This card doesn't belong to you")
            else skip;
            const card_pattern : card_pattern
            = case s.card_patterns [card.card_pattern] of [
                Some (pattern) -> pattern
              | None ->
                  (failwith ("sell_single: No card pattern.")
                   : card_pattern)
              ];
            card_pattern.quantity :=
              abs (SUB (card_pattern.quantity, 1n));
            const card_patterns : card_patterns
            = s.card_patterns;
            card_patterns [card.card_pattern] := card_pattern;
            s.card_patterns := card_patterns;
            const cards : cards = s.cards;
            const cards
            = Map.remove (action.card_to_sell, cards);
            s.cards := cards;
            const price : tez
            = TIMES
                (card_pattern.coefficient, card_pattern.quantity);
            const receiver : contract (unit)
            = case (Tezos.get_contract_opt (Tezos.sender)
                    : option (contract (unit)))
              of [
                Some (contract) -> contract
              | None ->
                  (failwith ("sell_single: No contract.")
                   : contract (unit))
              ];
            const op : operation
            = Tezos.transaction (unit, price, receiver);
            const operations : list (operation) = list [op]
          } with (operations, s)
      ]

    function buy_single
      (const gen__parameters3 : action_buy_single * storage) is
      case gen__parameters3 of [
        (action, s) ->
          block {
            const card_pattern : card_pattern
            = case s.card_patterns [action.card_to_buy] of [
                Some (pattern) -> pattern
              | None ->
                  (failwith ("buy_single: No card pattern.")
                   : card_pattern)
              ];
            const price : tez
            = TIMES
                (card_pattern.coefficient,
                 ADD (card_pattern.quantity, 1n));
            if GT (price, Tezos.amount)
            then failwith ("Not enough money")
            else skip;
            card_pattern.quantity :=
              ADD (card_pattern.quantity, 1n);
            const card_patterns : card_patterns
            = s.card_patterns;
            card_patterns [action.card_to_buy] := card_pattern;
            s.card_patterns := card_patterns;
            const cards : cards = s.cards;
            cards [s.next_id] :=
              record [
                card_owner = Tezos.sender;
                card_pattern = action.card_to_buy
              ];
            s.cards := cards;
            s.next_id := ADD (s.next_id, 1n)
          } with ((list [] : list (operation)), s)
      ]

    function main (const gen__parameters4 : parameter * storage) is
      case gen__parameters4 of [
        (action, s) ->
          case action of [
            Buy_single (bs) -> buy_single (bs, s)
          | Sell_single (as) -> sell_single (as, s)
          | Transfer_single (at) -> transfer_single (at, s)
          ]
      ] |}];
  run_ligo_good [ "transpile-contract" ; "../../test/contracts/coase.ligo" ; "cameligo" ] ;
  [%expect {|
    type card_pattern_id = nat

    type card_pattern = {coefficient : tez; quantity : nat}

    type card_patterns = (card_pattern_id, card_pattern) map

    type card_id = nat

    type card =
      {card_owner : address; card_pattern : card_pattern_id}

    type cards = (card_id, card) map

    type storage =
      {card_patterns : card_patterns;
       cards : cards;
       next_id : nat}

    type return = operation list * storage

    type action_buy_single = {card_to_buy : card_pattern_id}

    type action_sell_single = {card_to_sell : card_id}

    type action_transfer_single =
      {card_to_transfer : card_id; destination : address}

    type parameter =
      Buy_single of action_buy_single
    | Sell_single of action_sell_single
    | Transfer_single of action_transfer_single

    let transfer_single
    : action_transfer_single * storage -> return =
      (fun gen__parameters1 : action_transfer_single * storage ->
         match gen__parameters1 with
         action, s ->
             let cards : cards = s.cards in
             let card : card =
               match Map.find_opt action.card_to_transfer cards
               with
                 Some card -> card
               | None ->
                   ((failwith ("transfer_single: No card."))
                    : card) in
             begin
               if (NEQ (card.card_owner) (Tezos.sender))
               then
                 (failwith ("This card doesn't belong to you"))
               else ();
               let card =
                 {card with
                   {card_owner = action.destination}} in
               let cards =
                 Map.add card action.card_to_transfer cards in
               let s = {s with {cards = cards}} in
               ([] : operation list), s
             end)

    let sell_single : action_sell_single * storage -> return =
      (fun gen__parameters2 : action_sell_single * storage ->
         match gen__parameters2 with
         action, s ->
             let card : card =
               match Map.find_opt action.card_to_sell s.cards
               with
                 Some card -> card
               | None ->
                   ((failwith ("sell_single: No card.")) : card) in
             begin
               if (NEQ (card.card_owner) (Tezos.sender))
               then
                 (failwith ("This card doesn't belong to you"))
               else ();
               let card_pattern : card_pattern =
                 match Map.find_opt
                         card.card_pattern
                         s.card_patterns
                 with
                   Some pattern -> pattern
                 | None ->
                     ((failwith
                         ("sell_single: No card pattern."))
                      : card_pattern) in
               let card_pattern =
                 {card_pattern with
                   {quantity =
                      (abs ((SUB (card_pattern.quantity) (1n))))}} in
               let card_patterns : card_patterns =
                 s.card_patterns in
               let card_patterns =
                 Map.add
                   card_pattern
                   card.card_pattern
                   card_patterns in
               let s = {s with {card_patterns = card_patterns}} in
               let cards : cards = s.cards in
               let cards =
                 (Map.remove (action.card_to_sell) (cards)) in
               let s = {s with {cards = cards}} in
               let price : tez =
                 (TIMES
                    (card_pattern.coefficient)
                    (card_pattern.quantity)) in
               let receiver : unit contract =
                 match ((Tezos.get_contract_opt (Tezos.sender))
                        : unit contract option)
                 with
                   Some contract -> contract
                 | None ->
                     ((failwith ("sell_single: No contract."))
                      : unit contract) in
               let op : operation =
                 (Tezos.transaction (unit) (price) (receiver)) in
               let operations : operation list = [op] in
               operations, s
             end)

    let buy_single : action_buy_single * storage -> return =
      (fun gen__parameters3 : action_buy_single * storage ->
         match gen__parameters3 with
         action, s ->
             let card_pattern : card_pattern =
               match Map.find_opt
                       action.card_to_buy
                       s.card_patterns
               with
                 Some pattern -> pattern
               | None ->
                   ((failwith ("buy_single: No card pattern."))
                    : card_pattern) in
             let price : tez =
               (TIMES
                  (card_pattern.coefficient)
                  ((ADD (card_pattern.quantity) (1n)))) in
             begin
               if (GT (price) (Tezos.amount))
               then (failwith ("Not enough money"))
               else ();
               let card_pattern =
                 {card_pattern with
                   {quantity =
                      (ADD (card_pattern.quantity) (1n))}} in
               let card_patterns : card_patterns =
                 s.card_patterns in
               let card_patterns =
                 Map.add
                   card_pattern
                   action.card_to_buy
                   card_patterns in
               let s = {s with {card_patterns = card_patterns}} in
               let cards : cards = s.cards in
               let cards =
                 Map.add
                   {card_owner = Tezos.sender;
                    card_pattern = action.card_to_buy}
                   s.next_id
                   cards in
               let s = {s with {cards = cards}} in
               let s =
                 {s with
                   {next_id = (ADD (s.next_id) (1n))}} in
               ([] : operation list), s
             end)

    let main : parameter * storage -> return =
      (fun gen__parameters4 : parameter * storage ->
         match gen__parameters4 with
         action, s ->
             match action with
               Buy_single bs -> buy_single bs s
             | Sell_single as -> sell_single as s
             | Transfer_single at -> transfer_single at s) |}];
  run_ligo_good [ "transpile-contract" ; "../../test/contracts/coase.ligo" ; "reasonligo" ] ;
  [%expect {|
type card_pattern_id = nat;

type card_pattern = {coefficient: tez, quantity: nat };

type card_patterns = map(card_pattern_id, card_pattern);

type card_id = nat;

type card = {
  card_owner: address,
  card_pattern: card_pattern_id
};

type cards = map(card_id, card);

type storage = {card_patterns, cards, next_id: nat };

type return = (list(operation), storage);

type action_buy_single = {card_to_buy: card_pattern_id };

type action_sell_single = {card_to_sell: card_id };

type action_transfer_single = {
  card_to_transfer: card_id,
  destination: address
};

type parameter =
  Buy_single(action_buy_single)
| Sell_single(action_sell_single)
| Transfer_single(action_transfer_single);

let transfer_single
: (action_transfer_single, storage) => return =
  ((gen__parameters1: (action_transfer_single, storage))
   : return =>
     switch gen__parameters1{
     | action, s =>
         let cards: cards = s.cards;
         let card: card =
           switch
           Map.find_opt(action.card_to_transfer, cards){
           | Some card => card
           | None =>
               ((failwith(("transfer_single: No card.")))
                 : card)
           };
         {
           if ((NEQ((card.card_owner), (Tezos.sender)))) {
             (failwith(("This card doesn't belong to you")))
           } else {
             ()
           };
           let card =
             {...card,
               {card_owner: action.destination }};
           let cards =
             Map.add(card, action.card_to_transfer, cards);
           let s = {...s, {cards: cards }};
           ([] : list(operation)), s
         }
     });

let sell_single: (action_sell_single, storage) => return =
  ((gen__parameters2: (action_sell_single, storage)): return =>
     switch gen__parameters2{
     | action, s =>
         let card: card =
           switchMap.find_opt(action.card_to_sell, s.cards){
           | Some card => card
           | None =>
               ((failwith(("sell_single: No card."))) : card)
           };
         {
           if ((NEQ((card.card_owner), (Tezos.sender)))) {
             (failwith(("This card doesn't belong to you")))
           } else {
             ()
           };
           let card_pattern: card_pattern =
             switch
             Map.find_opt(card.card_pattern, s.card_patterns){
             | Some pattern => pattern
             | None =>
                 ((
                    failwith(("sell_single: No card pattern.")))
                   : card_pattern)
             };
           let card_pattern =
             {...card_pattern,
               {
                 quantity:
                   (
                    abs(((SUB((card_pattern.quantity), (1n))))))
               }};
           let card_patterns: card_patterns =
             s.card_patterns;
           let card_patterns =

             Map.add(card_pattern,
                card.card_pattern,
                card_patterns);
           let s = {...s, {card_patterns: card_patterns }};
           let cards: cards = s.cards;
           let cards =
             (Map.remove((action.card_to_sell), (cards)));
           let s = {...s, {cards: cards }};
           let price: tez =
             (
              TIMES((card_pattern.coefficient),
                 (card_pattern.quantity)));
           let receiver: contract(unit) =
             switch((Tezos.get_contract_opt((Tezos.sender)))
               : option(contract(unit))){
             | Some contract => contract
             | None =>
                 ((failwith(("sell_single: No contract.")))
                   : contract(unit))
             };
           let op: operation =
             (Tezos.transaction((unit), (price), (receiver)));
           let operations: list(operation) = [op];
           operations, s
         }
     });

let buy_single: (action_buy_single, storage) => return =
  ((gen__parameters3: (action_buy_single, storage)): return =>
     switch gen__parameters3{
     | action, s =>
         let card_pattern: card_pattern =
           switch
           Map.find_opt(action.card_to_buy, s.card_patterns){
           | Some pattern => pattern
           | None =>
               ((failwith(("buy_single: No card pattern.")))
                 : card_pattern)
           };
         let price: tez =
           (
            TIMES((card_pattern.coefficient),
               ((ADD((card_pattern.quantity), (1n))))));
         {
           if ((GT((price), (Tezos.amount)))) {
             (failwith(("Not enough money")))
           } else {
             ()
           };
           let card_pattern =
             {...card_pattern,
               {
                 quantity:
                   (ADD((card_pattern.quantity), (1n)))
               }};
           let card_patterns: card_patterns =
             s.card_patterns;
           let card_patterns =

             Map.add(card_pattern,
                action.card_to_buy,
                card_patterns);
           let s = {...s, {card_patterns: card_patterns }};
           let cards: cards = s.cards;
           let cards =

             Map.add({
                 card_owner: Tezos.sender,
                 card_pattern: action.card_to_buy
               },
                s.next_id,
                cards);
           let s = {...s, {cards: cards }};
           let s =
             {...s,
               {next_id: (ADD((s.next_id), (1n))) }};
           ([] : list(operation)), s
         }
     });

let main: (parameter, storage) => return =
  ((gen__parameters4: (parameter, storage)): return =>
     switch gen__parameters4{
     | action, s =>
         switch action{
         | Buy_single bs => buy_single(bs, s)
         | Sell_single as => sell_single(as, s)
         | Transfer_single at => transfer_single(at, s)
         }
     }); |}]

let%expect_test _ =
  run_ligo_good [ "transpile-contract" ; "../../test/contracts/deep_access.ligo" ; "pascaligo" ] ;
  [%expect{|
    type pii is int * int

    type ppi is record [x : pii; y : pii]

    type ppp is ppi * ppi

    function main (const toto : unit) is
    block {
      const a : ppp
      = (record [x = (0, 1); y = (10, 11)],
         record [x = (100, 101); y = (110, 111)]);
      a.0.x.0 := 2
    } with a.0.x.0

    function asymetric_tuple_access (const foo : unit) is
    block {
      const tuple : int * int * int * int = (0, (1, (2, 3)))
    } with
        ADD
          (ADD (ADD (tuple.0, tuple.1.0), tuple.1.1.0),
           tuple.1.1.1)

    type nested_record_t is
      record [nesty : record [mymap : map (int, string)]]

    function nested_record (const nee : nested_record_t) is
    block {
      nee.nesty.mymap [1] := "one"
    } with
        case nee.nesty.mymap [1] of [
          Some (s) -> s
        | None -> (failwith ("Should not happen.") : string)
        ] |}];
  run_ligo_good [ "transpile-contract" ; "../../test/contracts/deep_access.ligo" ; "cameligo" ] ;
  [%expect{|
    type pii = int * int

    type ppi = {x : pii; y : pii}

    type ppp = ppi * ppi

    let main : unit -> int =
      (fun toto : unit ->
         let a : ppp =
           {x = 0, 1; y = 10, 11}, {x = 100, 101; y = 110, 111} in
         let a = {a with {0.x.0 = 2}} in
         a.0.x.0)

    let asymetric_tuple_access : unit -> int =
      (fun foo : unit ->
         let tuple : int * int * int * int = 0, 1, 2, 3 in
         (ADD
            ((ADD ((ADD (tuple.0) (tuple.1.0))) (tuple.1.1.0)))
            (tuple.1.1.1)))

    type nested_record_t = {nesty : {mymap : (int, string) map}}

    let nested_record : nested_record_t -> string =
      (fun nee : nested_record_t ->
         let nee = Map.add "one" 1 nesty.mymap in
         match Map.find_opt 1 nee.nesty.mymap with
           Some s -> s
         | None -> ((failwith ("Should not happen.")) : string)) |}];
  run_ligo_good [ "transpile-contract" ; "../../test/contracts/deep_access.ligo" ; "reasonligo" ] ;
  [%expect{|
type pii = (int, int);

type ppi = {x: pii, y: pii };

type ppp = (ppi, ppi);

let main: unit => int =
  ((toto: unit): int =>
     let a: ppp =
       {
          x: 0, 1,
          y: 10, 11
        }, {x: 100, 101, y: 110, 111 };
     let a = {...a, {0.x[0]: 2 }};
     a[0].x[0]);

let asymetric_tuple_access: unit => int =
  ((foo: unit): int =>
     let tuple: (int, (int, (int, int))) = 0, 1, 2, 3;
     (
      ADD(((
          ADD(((ADD((tuple[0]), (tuple[1][0])))),
             (tuple[1][1][0])))),
         (tuple[1][1][1]))));

type nested_record_t = {nesty: {mymap: map(int, string) } };

let nested_record: nested_record_t => string =
  ((nee: nested_record_t): string =>
     let nee = Map.add("one", 1, nesty.mymap);
     switchMap.find_opt(1, nee.nesty.mymap){
     | Some s => s
     | None => ((failwith(("Should not happen."))) : string)
     }); |}]

let%expect_test _ =
(*
  run_ligo_good [ "transpile-contract" ; "../../test/contracts/double_fold_converter.religo" ; "pascaligo" ] ;
  [%expect{|
type tokenId is nat

type tokenOwner is address

type tokenAmount is nat

type transferContents is
  record [
    amount : tokenAmount;
    to_ : tokenOwner;
    token_id : tokenId
  ]

type transfer is
  record [from_ : tokenOwner; txs : list (transferContents)]

type transferContentsMichelson is
  michelson_pair_right_comb (transferContents)

type transferAuxiliary is
  record [
    from_ : tokenOwner;
    txs : list (transferContentsMichelson)
  ]

type transferMichelson is
  michelson_pair_right_comb (transferAuxiliary)

type transferParameter is list (transferMichelson)

type parameter is Transfer of transferParameter

type storage is big_map (tokenId, tokenOwner)

type entrypointParameter is parameter * storage

type entrypointReturn is list (operation) * storage

const errorTokenUndefined = "TOKEN_UNDEFINED"

const errorNotOwner = "NOT_OWNER"

const errorInsufficientBalance = "INSUFFICIENT_BALANCE"

type transferContentsIteratorAccumulator is
  storage * tokenOwner

function transferContentsIterator
  (const gen__1 :
     transferContentsIteratorAccumulator *
     transferContentsMichelson) is
block {
  const accumulator = gen__1.0;
  const transferContentsMichelson = gen__1.1;
  const gen__2 = accumulator;
  const storage = gen__2.0;
  const from_ = gen__2.1;
  const transferContents : transferContents
  = Layout.convert_from_right_comb
      (transferContentsMichelson);
  const tokenOwner : option (tokenOwner)
  = Map.find_opt (transferContents.token_id, storage);
  const tokenOwner
  = case tokenOwner of [
      Some (tokenOwner) ->
        if EQ (tokenOwner, from_)
        then tokenOwner
        else
          (failwith (errorInsufficientBalance) : tokenOwner)
    | None -> (failwith (errorTokenUndefined) : tokenOwner)
    ];
  const storage
  = Map.update
      (transferContents.token_id,
       Some (transferContents.to_), storage)
} with (storage, from_)

function allowOnlyOwnTransfer (const from : tokenOwner) is
  if NEQ (from, Tezos.sender)
  then failwith (errorNotOwner)
  else Unit

function transferIterator
  (const gen__3 : storage * transferMichelson) is
block {
  const storage = gen__3.0;
  const transferMichelson = gen__3.1;
  const transferAuxiliary2 : transferAuxiliary
  = Layout.convert_from_right_comb (transferMichelson);
  const from_ : tokenOwner = transferAuxiliary2.from_;
  allowOnlyOwnTransfer (from_);
  const gen__5
  = List.fold
      (transferContentsIterator, transferAuxiliary2.txs,
       (storage, from_));
  const storage = gen__5.0;
  const gen__4 = gen__5.1
} with storage

function transfer
  (const gen__6 : transferParameter * storage) is
block {
  const transferParameter = gen__6.0;
  const storage = gen__6.1;
  const storage
  = List.fold (transferIterator, transferParameter, storage)
} with ((list [] : list (operation)), storage)

function main (const gen__7 : entrypointParameter) is
block {
  const parameter = gen__7.0;
  const storage = gen__7.1
} with
    case parameter of [
      Transfer (transferParameter) ->
        transfer (transferParameter, storage)
    ] |}]; *)
  run_ligo_good [ "transpile-contract" ; "../../test/contracts/double_fold_converter.religo" ; "cameligo" ] ;
  [%expect{|
type tokenId = nat

type tokenOwner = address

type tokenAmount = nat

type transferContents =
  {amount : tokenAmount;
   to_ : tokenOwner;
   token_id : tokenId}

type transfer =
  {from_ : tokenOwner; txs : transferContents list}

type transferContentsMichelson =
  transferContents michelson_pair_right_comb

type transferAuxiliary =
  {from_ : tokenOwner; txs : transferContentsMichelson list}

type transferMichelson =
  transferAuxiliary michelson_pair_right_comb

type transferParameter = transferMichelson list

type parameter = Transfer of transferParameter

type storage = (tokenId, tokenOwner) big_map

type entrypointParameter = parameter * storage

type entrypointReturn = operation list * storage

let errorTokenUndefined = "TOKEN_UNDEFINED"

let errorNotOwner = "NOT_OWNER"

let errorInsufficientBalance = "INSUFFICIENT_BALANCE"

type transferContentsIteratorAccumulator =
  storage * tokenOwner

let transferContentsIterator =
  (fun gen__1 :
       transferContentsIteratorAccumulator *
       transferContentsMichelson ->
     let accumulator = gen__1.0 in
     let transferContentsMichelson = gen__1.1 in
     match accumulator with
     storage, from_ ->
         let transferContents : transferContents =
           (Layout.convert_from_right_comb
              (transferContentsMichelson)) in
         let tokenOwner : tokenOwner option =
           (Map.find_opt
              (transferContents.token_id)
              (storage)) in
         let tokenOwner =
           match tokenOwner with
             None ->
               ((failwith (errorTokenUndefined))
                : tokenOwner)
           | Some tokenOwner ->
               if (EQ (tokenOwner) (from_))
               then tokenOwner
               else
                 ((failwith (errorInsufficientBalance))
                  : tokenOwner) in
         let storage =
           (Map.update
              (transferContents.token_id)
              ((Some (transferContents.to_)))
              (storage)) in
         storage, from_)

let allowOnlyOwnTransfer =
  (fun from : tokenOwner ->
     if (NEQ (from) (Tezos.sender))
     then (failwith (errorNotOwner))
     else ())

let transferIterator =
  (fun gen__2 : storage * transferMichelson ->
     let storage = gen__2.0 in
     let transferMichelson = gen__2.1 in
     let transferAuxiliary2 : transferAuxiliary =
       (Layout.convert_from_right_comb (transferMichelson)) in
     let from_ : tokenOwner = transferAuxiliary2.from_ in
     begin
       allowOnlyOwnTransfer from_;
       match (List.fold
                (transferContentsIterator)
                (transferAuxiliary2.txs)
                (storage, from_))
       with
       storage, gen__3 -> storage
     end)

let transfer =
  (fun gen__4 : transferParameter * storage ->
     let transferParameter = gen__4.0 in
     let storage = gen__4.1 in
     let storage =
       (List.fold
          (transferIterator)
          (transferParameter)
          (storage)) in
     ([] : operation list), storage)

let main =
  (fun gen__5 : entrypointParameter ->
     let parameter = gen__5.0 in
     let storage = gen__5.1 in
     match parameter with
     Transfer transferParameter ->
         transfer transferParameter storage) |}];
  run_ligo_good [ "transpile-contract" ; "../../test/contracts/double_fold_converter.religo" ; "reasonligo" ] ;
  [%expect{|
type tokenId = nat;

type tokenOwner = address;

type tokenAmount = nat;

type transferContents = {
  amount: tokenAmount,
  to_: tokenOwner,
  token_id: tokenId
};

type transfer = {
  from_: tokenOwner,
  txs: list(transferContents)
};

type transferContentsMichelson = michelson_pair_right_comb
  (transferContents);

type transferAuxiliary = {
  from_: tokenOwner,
  txs: list(transferContentsMichelson)
};

type transferMichelson = michelson_pair_right_comb
  (transferAuxiliary);

type transferParameter = list(transferMichelson);

type parameter = Transfer(transferParameter);

type storage = big_map(tokenId, tokenOwner);

type entrypointParameter = (parameter, storage);

type entrypointReturn = (list(operation), storage);

let errorTokenUndefined = "TOKEN_UNDEFINED";

let errorNotOwner = "NOT_OWNER";

let errorInsufficientBalance = "INSUFFICIENT_BALANCE";

type transferContentsIteratorAccumulator = (storage,
 tokenOwner);

let transferContentsIterator =
  ((gen__1: (transferContentsIteratorAccumulator,
      transferContentsMichelson))
   : transferContentsIteratorAccumulator =>
     let accumulator = gen__1[0];
     let transferContentsMichelson = gen__1[1];
     switch accumulator{
     | storage, from_ =>
         let transferContents: transferContents =
           (
            Layout.convert_from_right_comb((transferContentsMichelson)));
         let tokenOwner: option(tokenOwner) =
           (
            Map.find_opt((transferContents.token_id),
               (storage)));
         let tokenOwner =
           switch tokenOwner{
           | None =>
               ((failwith((errorTokenUndefined)))
                 : tokenOwner)
           | Some tokenOwner =>
               if ((EQ((tokenOwner), (from_)))) {
                 tokenOwner
               } else {

                 ((failwith((errorInsufficientBalance)))
                   : tokenOwner)
               }
           };
         let storage =
           (
            Map.update((transferContents.token_id),
               ((Some((transferContents.to_)))),
               (storage)));
         storage, from_
     });

let allowOnlyOwnTransfer =
  ((from: tokenOwner): unit =>
     if ((NEQ((from), (Tezos.sender)))) {
       (failwith((errorNotOwner)))
     } else {
       ()
     });

let transferIterator =
  ((gen__2: (storage, transferMichelson)): storage =>
     let storage = gen__2[0];
     let transferMichelson = gen__2[1];
     let transferAuxiliary2: transferAuxiliary =
       (Layout.convert_from_right_comb((transferMichelson)));
     let from_: tokenOwner = transferAuxiliary2.from_;
     {
       allowOnlyOwnTransfer(from_);
       switch(
        List.fold((transferContentsIterator),
           (transferAuxiliary2.txs),
           (storage, from_))){
       | storage, gen__3 => storage
       }
     });

let transfer =
  ((gen__4: (transferParameter, storage)): entrypointReturn =>
     let transferParameter = gen__4[0];
     let storage = gen__4[1];
     let storage =
       (
        List.fold((transferIterator),
           (transferParameter),
           (storage)));
     ([] : list(operation)), storage);

let main =
  ((gen__5: entrypointParameter): entrypointReturn =>
     let parameter = gen__5[0];
     let storage = gen__5[1];
     switch parameter{
     | Transfer transferParameter =>
         transfer(transferParameter, storage)
     }); |}]

let%expect_test _ =
  run_ligo_good [ "transpile-contract" ; "../../test/contracts/FA1.2.ligo" ; "pascaligo" ] ;
  [%expect {|
    type tokens is big_map (address, nat)

    type allowances is big_map (address * address, nat)

    type storage is
      record [
        allowances : allowances;
        tokens : tokens;
        total_amount : nat
      ]

    type transfer is
      record [
        address_from : address;
        address_to : address;
        value : nat
      ]

    type approve is record [spender : address; value : nat]

    type getAllowance is
      record [
        callback : contract (nat);
        owner : address;
        spender : address
      ]

    type getBalance is
      record [callback : contract (nat); owner : address]

    type getTotalSupply is record [callback : contract (nat)]

    type action is
        Approve of approve | GetAllowance of getAllowance
      | GetBalance of getBalance
      | GetTotalSupply of getTotalSupply | Transfer of transfer

    function transfer
      (const gen__parameters1 : transfer * storage) is
      case gen__parameters1 of [
        (p, s) ->
          block {
            const new_allowances : allowances = big_map [];
            const gen__env10
            = record [new_allowances = new_allowances];
            const gen__env10
            = if EQ (Tezos.sender, p.address_from)
              then
                block {
                  const new_allowances = s.allowances;
                  gen__env10.new_allowances := new_allowances;
                  skip
                } with gen__env10
              else
                block {
                  const authorized_value : nat
                  = case Map.find_opt
                           ((Tezos.sender, p.address_from),
                            s.allowances)
                    of [
                      Some (value) -> value
                    | None -> 0n
                    ];
                  const gen__env9
                  = record [new_allowances = new_allowances];
                  const gen__env9
                  = if LT (authorized_value, p.value)
                    then
                      block {
                        failwith ("Not Enough Allowance")
                      } with gen__env9
                    else
                      block {
                        const new_allowances
                        = Map.update
                            ((Tezos.sender, p.address_from),
                             Some
                               (abs
                                  (SUB
                                     (authorized_value, p.value))),
                             s.allowances);
                        gen__env9.new_allowances :=
                          new_allowances;
                        skip
                      } with gen__env9;
                  const new_allowances
                  = gen__env9.new_allowances;
                  gen__env10.new_allowances := new_allowances;
                  skip
                } with gen__env10;
            const new_allowances = gen__env10.new_allowances;
            const sender_balance : nat
            = case Map.find_opt (p.address_from, s.tokens) of [
                Some (value) -> value
              | None -> 0n
              ];
            const new_tokens : tokens = big_map [];
            const gen__env13 = record [new_tokens = new_tokens];
            const gen__env13
            = if LT (sender_balance, p.value)
              then
                block {
                  failwith ("Not Enough Balance")
                } with gen__env13
              else
                block {
                  const new_tokens
                  = Map.update
                      (p.address_from,
                       Some
                         (abs (SUB (sender_balance, p.value))),
                       s.tokens);
                  gen__env13.new_tokens := new_tokens;
                  const receiver_balance : nat
                  = case Map.find_opt (p.address_to, s.tokens)
                    of [
                      Some (value) -> value
                    | None -> 0n
                    ];
                  const new_tokens
                  = Map.update
                      (p.address_to,
                       Some (ADD (receiver_balance, p.value)),
                       new_tokens);
                  gen__env13.new_tokens := new_tokens;
                  skip
                } with gen__env13;
            const new_tokens = gen__env13.new_tokens
          } with
              ((list [] : list (operation)),
               s with
                 record [
                   allowances = new_allowances;
                   tokens = new_tokens
                 ])
      ]

    function approve
      (const gen__parameters2 : approve * storage) is
      case gen__parameters2 of [
        (p, s) ->
          block {
            const previous_value : nat
            = case Map.find_opt
                     ((p.spender, Tezos.sender), s.allowances)
              of [
                Some (value) -> value
              | None -> 0n
              ];
            const new_allowances : allowances = big_map [];
            const gen__env16
            = record [new_allowances = new_allowances];
            const gen__env16
            = if AND (GT (previous_value, 0n), GT (p.value, 0n))
              then
                block {
                  failwith ("Unsafe Allowance Change")
                } with gen__env16
              else
                block {
                  const new_allowances
                  = Map.update
                      ((p.spender, Tezos.sender),
                       Some (p.value), s.allowances);
                  gen__env16.new_allowances := new_allowances;
                  skip
                } with gen__env16;
            const new_allowances = gen__env16.new_allowances
          } with
              ((list [] : list (operation)),
               s with
                 record [allowances = new_allowances])
      ]

    function getAllowance
      (const gen__parameters3 : getAllowance * storage) is
      case gen__parameters3 of [
        (p, s) ->
          block {
            const value : nat
            = case Map.find_opt
                     ((p.owner, p.spender), s.allowances)
              of [
                Some (value) -> value
              | None -> 0n
              ];
            const op : operation
            = Tezos.transaction (value, 0mutez, p.callback)
          } with (list [op], s)
      ]

    function getBalance
      (const gen__parameters4 : getBalance * storage) is
      case gen__parameters4 of [
        (p, s) ->
          block {
            const value : nat
            = case Map.find_opt (p.owner, s.tokens) of [
                Some (value) -> value
              | None -> 0n
              ];
            const op : operation
            = Tezos.transaction (value, 0mutez, p.callback)
          } with (list [op], s)
      ]

    function getTotalSupply
      (const gen__parameters5 : getTotalSupply * storage) is
      case gen__parameters5 of [
        (p, s) ->
          block {
            const total : nat = s.total_amount;
            const op : operation
            = Tezos.transaction (total, 0mutez, p.callback)
          } with (list [op], s)
      ]

    function main (const gen__parameters6 : action * storage) is
      case gen__parameters6 of [
        (a, s) ->
          case a of [
            Transfer (p) -> transfer (p, s)
          | Approve (p) -> approve (p, s)
          | GetAllowance (p) -> getAllowance (p, s)
          | GetBalance (p) -> getBalance (p, s)
          | GetTotalSupply (p) -> getTotalSupply (p, s)
          ]
      ] |}];
  run_ligo_good [ "transpile-contract" ; "../../test/contracts/FA1.2.ligo" ; "cameligo" ] ;
  [%expect {|
    type tokens = (address, nat) big_map

    type allowances = (address * address, nat) big_map

    type storage =
      {allowances : allowances;
       tokens : tokens;
       total_amount : nat}

    type transfer =
      {address_from : address;
       address_to : address;
       value : nat}

    type approve = {spender : address; value : nat}

    type getAllowance =
      {callback : nat contract;
       owner : address;
       spender : address}

    type getBalance = {callback : nat contract; owner : address}

    type getTotalSupply = {callback : nat contract}

    type action =
      Approve of approve | GetAllowance of getAllowance
    | GetBalance of getBalance
    | GetTotalSupply of getTotalSupply | Transfer of transfer

    let transfer
    : transfer * storage -> operation list * storage =
      (fun gen__parameters1 : transfer * storage ->
         match gen__parameters1 with
         p, s ->
             let new_allowances : allowances = Big_map.empty in
             let gen__env10 = {new_allowances = new_allowances} in
             let gen__env10 =
               if (EQ (Tezos.sender) (p.address_from))
               then
                 let new_allowances = s.allowances in
                 let gen__env10 =
                   {gen__env10 with
                     {new_allowances = new_allowances}} in
                 begin
                   ();
                   gen__env10
                 end
               else
                 let authorized_value : nat =
                   match (Map.find_opt
                            (Tezos.sender, p.address_from)
                            (s.allowances))
                   with
                     Some value -> value
                   | None -> 0n in
                 let gen__env9 =
                   {new_allowances = new_allowances} in
                 let gen__env9 =
                   if (LT (authorized_value) (p.value))
                   then
                     begin
                       (failwith ("Not Enough Allowance"));
                       gen__env9
                     end
                   else
                     let new_allowances =
                       (Map.update
                          (Tezos.sender, p.address_from)
                          ((Some
                              ((abs
                                  ((SUB
                                      (authorized_value)
                                      (p.value)))))))
                          (s.allowances)) in
                     let gen__env9 =
                       {gen__env9 with
                         {new_allowances = new_allowances}} in
                     begin
                       ();
                       gen__env9
                     end in
                 let new_allowances = gen__env9.new_allowances in
                 let gen__env10 =
                   {gen__env10 with
                     {new_allowances = new_allowances}} in
                 begin
                   ();
                   gen__env10
                 end in
             let new_allowances = gen__env10.new_allowances in
             let sender_balance : nat =
               match (Map.find_opt (p.address_from) (s.tokens))
               with
                 Some value -> value
               | None -> 0n in
             let new_tokens : tokens = Big_map.empty in
             let gen__env13 = {new_tokens = new_tokens} in
             let gen__env13 =
               if (LT (sender_balance) (p.value))
               then
                 begin
                   (failwith ("Not Enough Balance"));
                   gen__env13
                 end
               else
                 let new_tokens =
                   (Map.update
                      (p.address_from)
                      ((Some
                          ((abs
                              ((SUB (sender_balance) (p.value)))))))
                      (s.tokens)) in
                 let gen__env13 =
                   {gen__env13 with
                     {new_tokens = new_tokens}} in
                 let receiver_balance : nat =
                   match (Map.find_opt (p.address_to) (s.tokens))
                   with
                     Some value -> value
                   | None -> 0n in
                 let new_tokens =
                   (Map.update
                      (p.address_to)
                      ((Some
                          ((ADD (receiver_balance) (p.value)))))
                      (new_tokens)) in
                 let gen__env13 =
                   {gen__env13 with
                     {new_tokens = new_tokens}} in
                 begin
                   ();
                   gen__env13
                 end in
             let new_tokens = gen__env13.new_tokens in
             ([] : operation list),
             {s with
               {allowances = new_allowances;
                tokens = new_tokens}})

    let approve : approve * storage -> operation list * storage =
      (fun gen__parameters2 : approve * storage ->
         match gen__parameters2 with
         p, s ->
             let previous_value : nat =
               match (Map.find_opt
                        (p.spender, Tezos.sender)
                        (s.allowances))
               with
                 Some value -> value
               | None -> 0n in
             let new_allowances : allowances = Big_map.empty in
             let gen__env16 = {new_allowances = new_allowances} in
             let gen__env16 =
               if (AND
                     ((GT (previous_value) (0n)))
                     ((GT (p.value) (0n))))
               then
                 begin
                   (failwith ("Unsafe Allowance Change"));
                   gen__env16
                 end
               else
                 let new_allowances =
                   (Map.update
                      (p.spender, Tezos.sender)
                      ((Some (p.value)))
                      (s.allowances)) in
                 let gen__env16 =
                   {gen__env16 with
                     {new_allowances = new_allowances}} in
                 begin
                   ();
                   gen__env16
                 end in
             let new_allowances = gen__env16.new_allowances in
             ([] : operation list),
             {s with
               {allowances = new_allowances}})

    let getAllowance
    : getAllowance * storage -> operation list * storage =
      (fun gen__parameters3 : getAllowance * storage ->
         match gen__parameters3 with
         p, s ->
             let value : nat =
               match (Map.find_opt
                        (p.owner, p.spender)
                        (s.allowances))
               with
                 Some value -> value
               | None -> 0n in
             let op : operation =
               (Tezos.transaction (value) (0mutez) (p.callback)) in
             [op], s)

    let getBalance
    : getBalance * storage -> operation list * storage =
      (fun gen__parameters4 : getBalance * storage ->
         match gen__parameters4 with
         p, s ->
             let value : nat =
               match (Map.find_opt (p.owner) (s.tokens)) with
                 Some value -> value
               | None -> 0n in
             let op : operation =
               (Tezos.transaction (value) (0mutez) (p.callback)) in
             [op], s)

    let getTotalSupply
    : getTotalSupply * storage -> operation list * storage =
      (fun gen__parameters5 : getTotalSupply * storage ->
         match gen__parameters5 with
         p, s ->
             let total : nat = s.total_amount in
             let op : operation =
               (Tezos.transaction (total) (0mutez) (p.callback)) in
             [op], s)

    let main : action * storage -> operation list * storage =
      (fun gen__parameters6 : action * storage ->
         match gen__parameters6 with
         a, s ->
             match a with
               Transfer p -> transfer p s
             | Approve p -> approve p s
             | GetAllowance p -> getAllowance p s
             | GetBalance p -> getBalance p s
             | GetTotalSupply p -> getTotalSupply p s) |}];
  run_ligo_good [ "transpile-contract" ; "../../test/contracts/FA1.2.ligo" ; "reasonligo" ] ;
  [%expect {|
type tokens = big_map(address, nat);

type allowances = big_map((address, address), nat);

type storage = {allowances, tokens, total_amount: nat };

type transfer = {
  address_from: address,
  address_to: address,
  value: nat
};

type approve = {spender: address, value: nat };

type getAllowance = {
  callback: contract(nat),
  owner: address,
  spender: address
};

type getBalance = {callback: contract(nat), owner: address };

type getTotalSupply = {callback: contract(nat) };

type action =
  Approve(approve)
| GetAllowance(getAllowance)
| GetBalance(getBalance)
| GetTotalSupply(getTotalSupply)
| Transfer(transfer);

let transfer
: (transfer, storage) => (list(operation), storage) =
  ((gen__parameters1: (transfer, storage))
   : (list(operation), storage) =>
     switch gen__parameters1{
     | p, s =>
         let new_allowances: allowances = Big_map.empty;
         let gen__env10 = {
           new_allowances: new_allowances
         };
         let gen__env10 =
           if ((EQ((Tezos.sender), (p.address_from)))) {

             let new_allowances = s.allowances;
             let gen__env10 =
               {...gen__env10,
                 {new_allowances: new_allowances }};
             {
               ();
               gen__env10
             }
           } else {

             let authorized_value: nat =
               switch(
                Map.find_opt((Tezos.sender, p.address_from),
                   (s.allowances))){
               | Some value => value
               | None => 0n
               };
             let gen__env9 = {
               new_allowances: new_allowances
             };
             let gen__env9 =
               if ((LT((authorized_value), (p.value)))) {

                 {
                   (failwith(("Not Enough Allowance")));
                   gen__env9
                 }
               } else {

                 let new_allowances =
                   (
                    Map.update((Tezos.sender, p.address_from),
                       ((
                        Some(((
                            abs(((
                                SUB((authorized_value),
                                   (p.value)))))))))),
                       (s.allowances)));
                 let gen__env9 =
                   {...gen__env9,
                     {new_allowances: new_allowances }};
                 {
                   ();
                   gen__env9
                 }
               };
             let new_allowances = gen__env9.new_allowances;
             let gen__env10 =
               {...gen__env10,
                 {new_allowances: new_allowances }};
             {
               ();
               gen__env10
             }
           };
         let new_allowances = gen__env10.new_allowances;
         let sender_balance: nat =
           switch(Map.find_opt((p.address_from), (s.tokens))){
           | Some value => value
           | None => 0n
           };
         let new_tokens: tokens = Big_map.empty;
         let gen__env13 = {
           new_tokens: new_tokens
         };
         let gen__env13 =
           if ((LT((sender_balance), (p.value)))) {

             {
               (failwith(("Not Enough Balance")));
               gen__env13
             }
           } else {

             let new_tokens =
               (
                Map.update((p.address_from),
                   ((
                    Some(((
                        abs(((
                            SUB((sender_balance), (p.value)))))))))),
                   (s.tokens)));
             let gen__env13 =
               {...gen__env13,
                 {new_tokens: new_tokens }};
             let receiver_balance: nat =
               switch(
                Map.find_opt((p.address_to), (s.tokens))){
               | Some value => value
               | None => 0n
               };
             let new_tokens =
               (
                Map.update((p.address_to),
                   ((
                    Some(((
                        ADD((receiver_balance), (p.value))))))),
                   (new_tokens)));
             let gen__env13 =
               {...gen__env13,
                 {new_tokens: new_tokens }};
             {
               ();
               gen__env13
             }
           };
         let new_tokens = gen__env13.new_tokens;
         ([] : list(operation)),
          {...s,
            {
              allowances: new_allowances,
              tokens: new_tokens
            }}
     });

let approve
: (approve, storage) => (list(operation), storage) =
  ((gen__parameters2: (approve, storage))
   : (list(operation), storage) =>
     switch gen__parameters2{
     | p, s =>
         let previous_value: nat =
           switch(
            Map.find_opt((p.spender, Tezos.sender),
               (s.allowances))){
           | Some value => value
           | None => 0n
           };
         let new_allowances: allowances = Big_map.empty;
         let gen__env16 = {
           new_allowances: new_allowances
         };
         let gen__env16 =
           if ((
            AND(((GT((previous_value), (0n)))),
               ((GT((p.value), (0n))))))) {

             {
               (failwith(("Unsafe Allowance Change")));
               gen__env16
             }
           } else {

             let new_allowances =
               (
                Map.update((p.spender, Tezos.sender),
                   ((Some((p.value)))),
                   (s.allowances)));
             let gen__env16 =
               {...gen__env16,
                 {new_allowances: new_allowances }};
             {
               ();
               gen__env16
             }
           };
         let new_allowances = gen__env16.new_allowances;
         ([] : list(operation)),
          {...s,
            {allowances: new_allowances }}
     });

let getAllowance
: (getAllowance, storage) => (list(operation), storage) =
  ((gen__parameters3: (getAllowance, storage))
   : (list(operation), storage) =>
     switch gen__parameters3{
     | p, s =>
         let value: nat =
           switch(
            Map.find_opt((p.owner, p.spender),
               (s.allowances))){
           | Some value => value
           | None => 0n
           };
         let op: operation =
           (
            Tezos.transaction((value),
               (0mutez),
               (p.callback)));
         [op], s
     });

let getBalance
: (getBalance, storage) => (list(operation), storage) =
  ((gen__parameters4: (getBalance, storage))
   : (list(operation), storage) =>
     switch gen__parameters4{
     | p, s =>
         let value: nat =
           switch(Map.find_opt((p.owner), (s.tokens))){
           | Some value => value
           | None => 0n
           };
         let op: operation =
           (
            Tezos.transaction((value),
               (0mutez),
               (p.callback)));
         [op], s
     });

let getTotalSupply
: (getTotalSupply, storage) => (list(operation), storage) =
  ((gen__parameters5: (getTotalSupply, storage))
   : (list(operation), storage) =>
     switch gen__parameters5{
     | p, s =>
         let total: nat = s.total_amount;
         let op: operation =
           (
            Tezos.transaction((total),
               (0mutez),
               (p.callback)));
         [op], s
     });

let main: (action, storage) => (list(operation), storage) =
  ((gen__parameters6: (action, storage))
   : (list(operation), storage) =>
     switch gen__parameters6{
     | a, s =>
         switch a{
         | Transfer p => transfer(p, s)
         | Approve p => approve(p, s)
         | GetAllowance p => getAllowance(p, s)
         | GetBalance p => getBalance(p, s)
         | GetTotalSupply p => getTotalSupply(p, s)
         }
     }); |}]

let%expect_test _ =
  run_ligo_good [ "transpile-contract" ; "../../test/contracts/failwith.ligo" ; "pascaligo" ] ;
  [%expect {|
    type parameter is Pos of nat | Zero of nat

    type storage is unit

    type return is list (operation) * storage

    function main (const gen__parameters1 : parameter * storage) is
      case gen__parameters1 of [
        (p, s) ->
          block {
            case p of [
              Zero (n) ->
                if GT (n, 0n) then failwith ("fail") else skip
            | Pos (n) ->
                if GT (n, 0n) then skip else failwith ("fail")
            ]
          } with ((list [] : list (operation)), s)
      ]

    function foobar (const i : int) is
    block {
      const p : parameter = (Zero (42n));
      const gen__env8 = record [i = i];
      const gen__env8
      = if GT (i, 0)
        then
          block {
            const i = ADD (i, 1);
            gen__env8.i := i;
            const gen__env6 = record [i = i];
            const gen__env6
            = if GT (i, 10)
              then
                block {
                  const i = 20;
                  gen__env6.i := i;
                  failwith ("who knows");
                  const i = 30;
                  gen__env6.i := i;
                  skip
                } with gen__env6
              else
                block {
                  skip
                } with gen__env6;
            const i = gen__env6.i;
            gen__env8.i := i;
            skip
          } with gen__env8
        else
          block {
            case p of [
              Zero (n) -> failwith (42n)
            | Pos (n) -> skip
            ]
          } with gen__env8;
      const i = gen__env8.i
    } with
        case p of [
          Zero (n) -> i
        | Pos (n) -> (failwith ("waaaa") : int)
        ]

    function failer (const p : int) is
    block {
      if EQ (p, 1) then failwith (42) else skip
    } with p |}];
  run_ligo_good [ "transpile-contract" ; "../../test/contracts/failwith.ligo" ; "cameligo" ] ;
  [%expect {|
    type parameter = Pos of nat | Zero of nat

    type storage = unit

    type return = operation list * storage

    let main : parameter * storage -> return =
      (fun gen__parameters1 : parameter * storage ->
         match gen__parameters1 with
         p, s ->
             begin
               match p with
                 Zero n ->
                   if (GT (n) (0n))
                   then (failwith ("fail"))
                   else ()
               | Pos n ->
                   if (GT (n) (0n))
                   then ()
                   else (failwith ("fail"));
               ([] : operation list), s
             end)

    let foobar : int -> int =
      (fun i : int ->
         let p : parameter = (Zero 42n) in
         let gen__env8 = {i = i} in
         let gen__env8 =
           if (GT (i) (0))
           then
             let i = (ADD (i) (1)) in
             let gen__env8 = {gen__env8 with {i = i}} in
             let gen__env6 = {i = i} in
             let gen__env6 =
               if (GT (i) (10))
               then
                 let i = 20 in
                 let gen__env6 = {gen__env6 with {i = i}} in
                 begin
                   (failwith ("who knows"));
                   let i = 30 in
                   let gen__env6 = {gen__env6 with {i = i}} in
                   begin
                     ();
                     gen__env6
                   end
                 end
               else
                 begin
                   ();
                   gen__env6
                 end in
             let i = gen__env6.i in
             let gen__env8 = {gen__env8 with {i = i}} in
             begin
               ();
               gen__env8
             end
           else
             begin
               match p with
                 Zero n -> (failwith (42n))
               | Pos n -> ();
               gen__env8
             end in
         let i = gen__env8.i in
         match p with
           Zero n -> i
         | Pos n -> ((failwith ("waaaa")) : int))

    let failer : int -> int =
      (fun p : int ->
         begin
           if (EQ (p) (1)) then (failwith (42)) else ();
           p
         end) |}];
  run_ligo_good [ "transpile-contract" ; "../../test/contracts/failwith.ligo" ; "reasonligo" ] ;
  [%expect {|
type parameter = Pos(nat) | Zero(nat);

type storage = unit;

type return = (list(operation), storage);

let main: (parameter, storage) => return =
  ((gen__parameters1: (parameter, storage)): return =>
     switch gen__parameters1{
     | p, s =>
         {
           switch p{
           | Zero n =>
               if ((GT((n), (0n)))) {
                 (failwith(("fail")))
               } else {
                 ()
               }
           | Pos n =>
               if ((GT((n), (0n)))) {
                 ()
               } else {
                 (failwith(("fail")))
               }
           };
           ([] : list(operation)), s
         }
     });

let foobar: int => int =
  ((i: int): int =>
     let p: parameter = (Zero 42n);
     let gen__env8 = {
       i: i
     };
     let gen__env8 =
       if ((GT((i), (0)))) {

         let i = (ADD((i), (1)));
         let gen__env8 = {...gen__env8, {i: i }};
         let gen__env6 = {
           i: i
         };
         let gen__env6 =
           if ((GT((i), (10)))) {

             let i = 20;
             let gen__env6 = {...gen__env6, {i: i }};
             {
               (failwith(("who knows")));
               let i = 30;
               let gen__env6 = {...gen__env6, {i: i }};
               {
                 ();
                 gen__env6
               }
             }
           } else {

             {
               ();
               gen__env6
             }
           };
         let i = gen__env6.i;
         let gen__env8 = {...gen__env8, {i: i }};
         {
           ();
           gen__env8
         }
       } else {

         {
           switch p{
           | Zero n => (failwith((42n)))
           | Pos n => ()
           };
           gen__env8
         }
       };
     let i = gen__env8.i;
     switch p{
     | Zero n => i
     | Pos n => ((failwith(("waaaa"))) : int)
     });

let failer: int => int =
  ((p: int): int => {
     if ((EQ((p), (1)))) {
       (failwith((42)))
     } else {
       ()
     };
     p
   }); |}]

let%expect_test _ =
  run_ligo_good [ "transpile-contract" ; "../../test/contracts/recursion.ligo" ; "pascaligo" ] ;
  [%expect {|
    recursive function sum (const gen__parameters1 : int * int) is
      case gen__parameters1 of [
        (n, acc) ->
          if LT (n, 1)
          then acc
          else sum (SUB (n, 1), ADD (acc, n))
      ]

    recursive function fibo
      (const gen__parameters2 : int * int * int) is
      case gen__parameters2 of [
        (n, n_1, n_0) ->
          if LT (n, 2)
          then n_1
          else fibo (SUB (n, 1), ADD (n_1, n_0), n_1)
      ] |}];
  run_ligo_good [ "transpile-contract" ; "../../test/contracts/recursion.ligo" ; "cameligo" ] ;
  [%expect {|
    let rec sum : int * int -> int =
      (fun gen__parameters1 : int * int ->
         match gen__parameters1 with
         n, acc ->
             if (LT (n) (1))
             then acc
             else sum (SUB (n) (1)) (ADD (acc) (n)))

    let rec fibo : int * int * int -> int =
      (fun gen__parameters2 : int * int * int ->
         match gen__parameters2 with
         n, n_1, n_0 ->
             if (LT (n) (2))
             then n_1
             else fibo (SUB (n) (1)) (ADD (n_1) (n_0)) n_1) |}];
  run_ligo_good [ "transpile-contract" ; "../../test/contracts/recursion.ligo" ; "reasonligo" ] ;
  [%expect {|
    let rec sum: (int, int) => int =
      ((gen__parameters1: (int, int)): int =>
         switch gen__parameters1{
         | n, acc =>
             if ((LT((n), (1)))) {
               acc
             } else {
               sum((SUB((n), (1))), (ADD((acc), (n))))
             }
         });

    let rec fibo: (int, int, int) => int =
      ((gen__parameters2: (int, int, int)): int =>
         switch gen__parameters2{
         | n, n_1, n_0 =>
             if ((LT((n), (2)))) {
               n_1
             } else {
               fibo((SUB((n), (1))), (ADD((n_1), (n_0))), n_1)
             }
         }); |}]
