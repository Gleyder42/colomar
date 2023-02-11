# Native elements

A native element can be a function, a struct or an enum. They directly translate  Workshop Code. Native functions are only declared in Colomar. Their implementation is stated in a separate file.

```
pub native struct Player {
    pub native var ability1Enabled: Bool
    pub native val hero: Hero
    pub native val isFiringSecondary: Bool

    pub native fn heal(amount: number, healer: Player = null)

    pub native fn applyImpulse(direction: Vector, speed: Number, relative: Relative, motion: Motion)
}
```



## Native Rule Events

```` 
pub native event PlayerDealtFinalBlow(team: Team, heroSlot: HeroSlot) {
    val player: Player
    val attacker: Player
    val victim: Player
}

pub native event OngoingEachPlayer(team: Team, heroSlot: HeroSlot) {
    val player: Player
}
````

## Native Enums

```
pub native enum Team {
    Team1, Team2, All
}
```

