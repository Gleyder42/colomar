# Rules

A rule defines a behavior when an event occurs.
A rule has a name, an event type and multiple conditions and actions.
The rule executes the actions only if all conditions are true and were previously false.

Example:
We define a rule, 
which heals the player (the action) 
when he kills another player (the event), 
but only if the killer is reloading (the condition)

Rule Structure:
A rule consists of multiple elements.

- `"Heal on kill"` is the rule name
- `PlayerDealtFinalBlow` is the event
- `Team1` and `Slot1` are parameter values
- `cond player.isReloading` is the condition
- `player.heal(100)` is the action

### Declaring a rule

```
rule "Heal on kill" PlayerDealtFinalBlow(Team.Team1, HeroSlot.Slot1) { 
    cond player.isReloading
    
    player.heal(100)
}
```

### Declaring Custom Events

```
event PlayerWeaponReloading(heroSlot: HeroSlot) by OngoingEachPlayer(Team.All, hero) {
    val player = by.player
    
    cond player.isReloading
    self()
}

rule "On Weapon Reload" WeaponReload(HeroSlot.Ana) {
    player.ability1Enabled = false
}
```



## Workshop Event List

Colomar names are workshop names with the spaces replaced. Only `Ongoing` events are the exception.

| Workshop name             | Colomar name            | Parameters     | Context variables        |
| ------------------------- | ----------------------- | -------------- | ------------------------ |
| Ongoing - Global          | OngoingGlobal           | None           | None                     |
| Ongoing - Each Player     | OngoingEachPlayer       | Team, HeroSlot | player                   |
| Player Earned Elimination | PlayerEarnedElimination | Team, HeroSlot | player, attacker, victim |
| Player Dealt Final Blow   | PlayerDealtFinalBlow    | Team, HeroSlot | player, attacker, victim |
| Player Dealt Damage       | PlayerDealtDamage       | Team, HeroSlot | player, attacker, victim |
| Player Took Damage        | PlayerTookDamage        | Team, HeroSlot | player, attacker, victim |
| Player Died               | PlayerDied              | Team, HeroSlot | player, attacker, victim |
| Player Dealt Healing      | PlayerDealtHealing      | Team, HeroSlot | player, healee, healer   |
| Player Received Healing   | PlayerReceivedHealing   | Team, HeroSlot | player, healee, healer   |
| Player Joined Match       | PlayerJoinedMatch       | Team, HeroSlot | player                   |
| Player Left Match         | PlayerLeftMatch         | Team, HeroSlot | player                   |
| Player Dealt Knockback    | PlayerDealtKnockback    | Team, HeroSlot | player, attacker, victim |
| Player Received Knockback | PlayerReceivedKnockback | Team, HeroSlot | player, attacker, victim |

## Team List

| Team  |
| ----- |
| Team1 |
| Team2 |
| All   |

## HeroSlot List

| HeroSlot                                                     |
| ------------------------------------------------------------ |
| Slot0, Slot1, Slot2, Slot3, Slot4, Slot5, Slot6, Slot7, Slot8, Slot9, Slot10, Slot11, All |
| Reaper, Tracer, Mercy, Hanzo, Torbjörn, Reinhardt, Pharah, Winston,  Widowmaker, Bastion, Symmetra, Zenyatta, Genji, Roadhog, Cassidy, Junkrat, Zarya, Soldier76, Lucio, Dva, Mei, Sombra, Doomfist, Ana, Orisa, Brigitte, Moria, WreckingBall, Ashe, Echo, Baptise, Sigma, All |

### Ongoing - Each Player

Executes if the conditions went false and subsequently true again.

Executes if a player joins a match.

Executes if a player switches the team.

------

