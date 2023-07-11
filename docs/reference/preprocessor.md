# Preprocessor

```
rule "Heal on kill" PlayerDealtFinalBlow(Team1, Slot1) {
    cond player.isReloading
    
    #if DEBUG
    player.smallMessage("Player healed")
    #endif
    
    player.heal(100)
}
```

