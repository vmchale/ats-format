# Features
- [ ] vim plugin
- [ ] don't rewrite `absvtype` to `absviewtype`?
- [ ] don't rewrite empty braces?
# Performance
- [ ] consider bytestring lexer
- [ ] Consider a different pretty-printer
- [ ] Read Alex manual (particularly contexts & start codes)
  - [ ] contextual matching for lambda arrows.
  - [ ] use monadic lexer
# Design
- [ ] align `=` in records?
- [ ] figure out error handling in `happy`
- [ ] handle location information in a commonad?
# Bugs
- [ ] `Arg` should include proofs, e.g. `ptr_get0<a> (pf1 | p1)`.
- [ ] ATS parses `if` statements differently than we do (else clause isn't
  necessary)
- [ ] macdef
- [ ] `:<>` and the like
- [ ] `vtypdef` should work on more inputs
- [ ] `datavtype` should be more general (dependent types)
- [ ] `assume`
- [ ] `castfn`
- [ ] `propdef`
- [ ] `abstype`
- [ ] `type`
- [ ] `view`
- [ ] `prfn`
- [ ] `tkindef` / `typekindef`?
- [ ] `prfun`
- [ ] `primplmnt'
- [ ] `implmnt`
- [ ] `infixr`
- [ ] `absvt@ype`
- [ ] `$effmask\_wrt` and the like
- [ ] everything in this list: https://github.com/githwxi/ATS-Postiats/blob/d28486e9fb1954f41521a464d37f6bfba57b7a91/utils/emacs/ats2-mode.el#L270
- [ ] `list\_vt{int}(0, 1, 2, 3, 4)` (list/array literals)
- [ ] preserve comments
- [ ] nested block comments
- [ ] https://github.com/githwxi/ATS-Postiats/wiki/effects
# Code maintenance
- [ ] refactor such that *all* types return location information, write `HasLoc`
  class, and use it to make failures more precise
- [ ] stateful lexer (particularly for `<`)
- [ ] data type for type declarations
- [ ] have an actual sane way to handle fixity
