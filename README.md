Compiler for a little toy language, made for WPI's CS4533 (Prog Lang
Translation), as per *Essentials of Compilation: An Incremental Approach
in Python* by Jeremy G. Siek. I decided to write it from scratch in Rust
instead of using the provided Python infrastructure though.

Todo's:
- Refactor to pass each X86Program function individually to each pass
- Add a CLI input/output (probably needs a parser first)
- The rest of the f**king compiler