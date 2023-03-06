# Workshop elements

A workshop element can be a function, a struct or an enum. 
They directly translate to Workshop Code. 
Workshop functions are declared in Colomar and defined in a separate file.

```
pub workshop struct Player {
    pub workshop var ability1Enabled: Bool
    pub workshop val hero: Hero
    pub workshop val isFiringSecondary: Bool

    pub workshop fn heal(self, amount: number, healer: Player = null)

    pub workshop fn applyImpulse(self, direction: Vector, speed: Number, relative: Relative, motion: Motion)
}
```

## Workshop Rule Events

```` 
pub workshop event PlayerDealtFinalBlow(team: Team, heroSlot: HeroSlot) {
    val player: Player
    val attacker: Player
    val victim: Player
}

pub workshop event OngoingEachPlayer(team: Team, heroSlot: HeroSlot) {
    val player: Player
}
````

## Workshop Enums

```
pub workshop enum Team {
    Team1, Team2, All
}
```

