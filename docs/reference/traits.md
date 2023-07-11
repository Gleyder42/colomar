# Traits

```` 
trait OnReloadAction<H> {
	
	fn run(player: Player)
}

impl OnReloadAction<Ana> for Hero {
	
	fn run(player: Player) {
		// code...
	}
}

rule "Do something on reload" WeaponReload() {
	player.hero.run(player)
}
````

