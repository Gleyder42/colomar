# Introduction

_Requires previous Overwatch Workshop knowledge_

Before starting, lets define terminology.
The raw Overwatch workshop code (the one that is understood by Overwatch directly) is from here on referred as wscript.
This is short for
**w**orkshop **script**.

Colomar is driven by two core ideas:

## Make Colomar OOP like (but not fully)

That means instead of writing something like

```
smallMessage(eventPlayer(), "Hello World")
```

we write something like

```
player.sendSmallMessage("Hello World")
```

Doing this

- we convert that method into an object method, to indicate that the message is called on the `player` variable
- we add a `send` in front of `smallMessage` to get a proper naming convention (here verb + noun)
- we assume that a variable `player` points to the event player.

So in conclusion,

```
player.sendSmallMessage("Hello World")
```

transpiles to

```
Small Message(Event Player, Custom String("Hello World"));
```

in wscript.

This design philosophy stands out from the other Overwatch DSL languages
like [Overpy](https://github.com/Zezombye/overpy), [Overwatch-Script-To-Workshop](https://github.com/ItsDeltin/Overwatch-Script-To-Workshop)
and [Overwatch Workshop Script Editor](https://workshop.codes/editor), which keep the wscript like syntax and name
conventions

--- 

## Allow easy interaction between Colomar and wscript

To explain that, lets take our code snippet ```player.sendSmallMessage("Hello World")``` from the previous explanation.
First, we have `player` here, that means somewhere is defined that `player` transpiles to `Event Player` in wscript.
And this information is not hidden in the compiler internals, but available to the Colomar programmer.
It also means, that `sendSmallMessage` is somewhere defined as `Small Message`.


