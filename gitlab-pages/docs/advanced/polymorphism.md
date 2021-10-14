---
id: polymorphism
title: Polymorphism
---

import Syntax from '@theme/Syntax';
import Link from '@docusaurus/Link';

LIGO supports simple polymorphism when introducing declarations. This
allows to write functions parametric on a type that can be later
instantiated to particular types.

## The identity function

For any given type `t`, there is a canonical function of type `t -> t`
(function from `t` to `t`): it takes an argument, and returns it
straight away. For example, we can write the identity function for
`int`:

<Syntax syntax="pascaligo">

```pascaligo group=mono
function id (const x : int) : int is
  x
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=mono
let id (x : int) = x
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=mono
let id = (x : int) : int => x;
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=mono
let id = (x: int): int => x;
```

</Syntax>

However, if we would want to use the same function on a different
type, e.g., on `nat`, then we will need a new definition:

<Syntax syntax="pascaligo">

```pascaligo group=mono
function idnat (const x : nat) : nat is
  x
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=mono
let idnat (x : nat) = x
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=mono
let idnat = (x : nat) : nat => x;
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=mono
let idnat = (x: nat): nat => x;
```

</Syntax>

If we see in detail, there is almost no difference between `id` and
`idnat`: it's just the type that changes, but for the rest, the body
of the function stays the same.

Parametric polymorphism allows us to write a single function
declaration that works for both cases.

<Syntax syntax="pascaligo">

```pascaligo group=poly
function id (const x : _a) : _a is
  x
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=poly
let id (type a) (x : a) : a = x
```

Here we introduce an abstract type variable `a` which can be
generalized using `(type a)` in the declaration. Alternatively, we
could write:

```cameligo group=poly2mligo
let id (x : _a) : _a = x
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=poly
let id : (_a => _a) = (x : _a) => x;
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=poly
let id : ((x : _a) => _a) = (x: _a) => x;
```

</Syntax>

Here `_a` is a type variable which can be generalized. In general,
types prefixed with `_` are treated as generalizable.

We can then use this function directly in different types by just
regular application:

<Syntax syntax="pascaligo">

```pascaligo group=poly
const three_i : int = id(3);
const three_s : string = id("three");
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=poly
let three_i : int = id 3
let three_s : string = id "three"
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=poly
let three_i : int = id(3);
let three_s : string = id("three");
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=poly
const three_i : int = id(3);
const three_s : string = id("three");
```

</Syntax>
