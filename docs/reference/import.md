# Import

You can import other files, which brings all element from that file into the current scope.

## Syntax

```text
Path = Ident [ "::" Path ]; 
Import = "import" Path ;
```

## Behavior

All public elements from the imported file are treated as they were defined in the current file.
- Duplicate files imported by transitive files are ignored.
- A file cannot import itself.
- The order of imports itself or where imports are place inside the file does not matter.
- Two files can import each other
- Only public elements are brought into scope

## Constructing paths

Paths are based on the current directory.
If the current directory is structured as following:

```text
src
  debug
    file.co
    test.co
  main.co
  a.co
```

Then you have to use the following statements to import, while inside `main.co`.

```text
import a;
import debug::file;
import test::co;
```
