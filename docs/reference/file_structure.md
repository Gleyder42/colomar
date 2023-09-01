# File Structure

The root directory must have a `src` directory and should have a `native` directory.

The `src` directory
- must have a file named `main.co`.
- may have more files and directories.

The `native` directory may have `struct`, `event` or `enum` directory.
Each directory may have multiple `.toml` files.

## Examples

```text  
native
  enum
    Hero.toml
    Slot.toml
    Team.toml
  event
    OngoingEachPlayer.toml
  struct
    Player.toml
src
  main.co
```