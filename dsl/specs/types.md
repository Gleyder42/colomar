# Variables

Immutable types are declared by the `val` keyword.

```
val foo: Number = 10
val bar: String = "Hello World"
val isSometing: Boolean = false
```

The type can be omitted if it can be inferred from the context.

```
val foo = 10
val bar = "Hello World"
val isSometing = false
```

Mutable types are declared by the `var` keyword.

```
var foo = 10
var bar = "Hello World"
var isSometing = false

foo = 20
bar = "Test"
isSomething = true
```