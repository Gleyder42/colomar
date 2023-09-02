# Struct

Structs are used to group common properties and functions together.

Member functions take self as a first parameter.
They have access to private fields and functions.
You can use extension methods to extend a closed class.
Although you use `self` you don't have access to private fields or functions.

```text
struct Zone {
    
    var a: Vector
    var b: Vector
}
```
### Open Structs

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

