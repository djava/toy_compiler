Compiler for a little toy language, made for WPI's CS4533 (Prog Lang
Translation), as per *Essentials of Compilation: An Incremental Approach
in Python* by Jeremy G. Siek. I decided to write it from scratch in Rust
instead of using the provided Python infrastructure though.

Todo's:
- Kill the `String`s in the AST to get rid of all the `.clone()`s
- Refactor to pass each X86Program function individually to each pass
- Design a pass pipeline mechanism
- Add a CLI input/output (probably needs a parser first)
- The rest of the f**king compiler