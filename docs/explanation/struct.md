# Struct

Structs are used to group common properties and functions together.

## Regular structs


Struct declaration and definition.
```text
struct Data {
    val name: string;
    val amount: num;
    val isGood: bool;
}
```

Struct initialization.
```text
// colomar
val a: Data = Data { name: "Hello World", amount: 10, isGood: true };
val b: &Data = new Data { name: "Hello World", amount: 10, isGood: true };

// workshop
Event Player.a = Array("Hello World", 10, True);

Global.heap[0] = Array("Hell74o World", 10, True);
Event Player.b = 
```

```text
struct Zone {
    var a: vector;
    var b: vector;
}
```

```text

```

## Native Structs

Native structs group workshop functions within a similar context.
Native structs are intended to simulate struct like behavior.

Take the `Vector` native struct for example:

```text
native struct vector {
    val x: num;
    val y: num;
    val z: num;
    
    native fn angleBetween(other: Vector) -> Vector;
}
```

The `Vector` struct defines three properties: `x, y, z`.
They are of type `getval`, which means once the struct is created, you cannot change the value anymore.
This is commonly referred to as immutability.

## Open Structs

Open structs are declared with the `open` keyword.
That means you can add methods and properties to the method while having
access to private fields.

```
open struct Handler {
    val x: Vector
}

open struct Handler {
    val y: string
}
```

### Workshop Representation

