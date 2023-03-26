# Functions

Functions are inlined per default.
Functions can have parameters.
Parameters can have default arguments.

```
fn calculate(player: Player, hero: Hero, message: String) -> Num {

}
```

Adding `subr` makes the function a subroutine.

```
fn subr calculate(player: &Player, hero: Hero, message: &String) -> num {
	
}
```

Adding `value` makes the function usable in conditions. 
The function must return a value and only `value-funtions` are allowed.

```
fn value calculate(player: &Player, hero: Hero, message: Str) -> num {
	
}
```
