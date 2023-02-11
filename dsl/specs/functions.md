# Functions

Normal functions are inlined.
Functions can have parameters.
Parameters can have default arguments.

```
fn calculate(player: Player, hero: Hero, message: String = "Hello Worlds") -> Number {

}
```

Adding `subr` makes the function a subroutine.

```
fn subr calculate(player: Player, hero: Hero, message: String) -> Number {
	
}
```

Adding `value` makes the function usable in conditions. 
The function must return a value and only `value-funtions` are allowed.

```
fn value calculate(player: Player, hero: Hero, message: String) -> Number {
	
}
```
