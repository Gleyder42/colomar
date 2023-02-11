```
struct Zone {
    val pos1: Vector
    val pos2: Vector
}

player {
    val zone: ref Zone
}

global {
    val zone: Zone
}
```