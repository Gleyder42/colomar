# Syntax

```
"let" "mut"? (":" Type)? ("=" AValue)

let temp = "Test"
```

# Transpiled Workshop Code

```
variables
{
	global:
		0: Hello
}

rule("Rule 1")
{
	event
	{
		Ongoing - Each Player;
		All;
		All;
	}

	actions
	{
		Global.Hello = 0;
	}
}
```

# Description

Variables are used to store data.
Variables are declared using the `let` keyword.
You can omit the type if it can be inferred from the context.

```
val age = 10;
let name = "Hello World"

name = "Goodbye" // error, variable is immutable by default
```

You can also explicitly state the type.

```
let counter: num = 5;
let hero: Hero = Hero.Ana;
```

Variables are immutable by default.
To make the mutable, use the `mut` keyword.

```
let mut counter = 10;
counter++;

counter = counter + 10;
```

If a variable is immutable you can't change the value itself or any field of a struct.

```
struct Data {
    let name: string
    let age: number
    let isDirty: bool
}

let data = Data { name: "Hello World", age: 10.5 isDirty: false }

data.isDirty = true; // error, data variable is immutable  
```