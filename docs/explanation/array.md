# Array

Functions modifying arrays, always copy the array.
This introduces boilerplate code, because after every operation the new array must be assigned to a variable.
It can also lead to bugs, where the changed array was not assigned to a variable.


### Create Array
```text
val array: array<num> = [1, 2, 3, 4];
var array2: array<num> = array.new<Test>(1, 2, 3, 4)
``` 

### Array definition
```text
struct array<T> {
    
    native static fn new(vararg elements: num);
    
    native fn add(element: num);
}
```

### Array operations
```text
var array: array<num> = [];
array.add(5);
```
