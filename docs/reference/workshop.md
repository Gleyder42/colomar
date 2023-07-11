# Workshop elements

A workshop element can be a function, a struct or an enum. 
They directly translate to Workshop Code. 
Workshop functions are declared in Colomar and defined in a separate file.

```
pub native struct Player {
    pub native var ability1Enabled: Bool
    pub native val hero: Hero
    pub native val isFiringSecondary: Bool

    pub native fn heal(self, amount: number, healer: Player = null)

    pub native fn applyImpulse(self, direction: Vector, speed: Number, relative: Relative, motion: Motion)
}
```

## Workshop Rule Events

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

## Workshop Enums

```
pub native enum Team {
    Team1, Team2, All
}
```

