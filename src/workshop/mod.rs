mod interpreter;

struct Player(String);

struct Team(String);

struct Event(String);

struct Rule {
    name: String,
    event: Event,
    team: Team,
    player: Player
}
