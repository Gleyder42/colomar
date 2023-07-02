# Functions

```
fn test(a: num) { }

test()

fn hello(a: num = 10, b: num = 20, c = 30) -> num {
}

fn hello(1, 2, 3) // 1, 2, 3 
fn hello(1) // 1, 20, 30
fn hello(c = 5) // 10, 20, 5
fn hello(1, c = 5) // 1, 20, 5
fn hello(c = 10, b = 10)
fn hello(a = 1, 2, 3)


fn hello(7, a = 5)
fn hello(c = 5, 1)
fn hello(c = 1, 2, a = 3)

fn hello(1, 2, 3, 4)
```