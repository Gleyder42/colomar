# References

References point to an existing variable

```
var score: &num // global scope

fn test() {
    var test = 10
    
    score = &test // local scope
    
    inc(&test)
}

fn inc(out: &var num) {
    num++
}
```