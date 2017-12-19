# Features
- [ ] call `clang-format` on the inserted C.
- [x] `.atsfmt.toml` to configure pretty-printer
  - [ ] set project files in `.atsfmt.toml`
  - [ ] automatically generate a sample configuration
- [ ] should intelligently decide when to line break based on various things.
- [ ] errors should be displayed more nicely
- [ ] vim plugin
# Performance
- [ ] Try to get rid of conflicts
- [ ] Consider a different pretty-printer
- [ ] Read Alex manual (particularly contexts & start codes)
  - [ ] contextual matching for lambda arrows.
  - [ ] use monadic lexer
# Design
- [ ] align `=` in records?
# Bugs
- [ ] `Arg` should include proofs, e.g. `ptr_get0<a> (pf1 | p1)`.
- [ ] ATS parses `if` statements differently?
- [ ] `sif`
- [ ] macdef
# Code maintenance
- [ ] refactor such that *all* types return location information, write `HasLoc`
  class, and use it to make failures more precise
- [ ] stateful lexer (particularly for `<`)
