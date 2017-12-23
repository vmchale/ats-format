# Features
- [ ] vim plugin
- [ ] don't rewrite `absvtype` to `absviewtype`?
- [ ] don't rewrite empty braces?
- [ ] align comments w/ nearest block
- [ ] rewrite (a; b; c;) using begin.. end blocks
# Performance
- [ ] consider bytestring lexer
- [ ] Consider a different pretty-printer
- [ ] Read Alex manual (particularly contexts & start codes)
  - [ ] contextual matching for lambda arrows.
  - [ ] use monadic lexer
# Design
- [ ] align `=` in records?
- [ ] monadic parser for error handling?
- [ ] `Control.Lens.Plated` to rewrite nested binary operations?
- [ ] handle location information in a comonad?
- [ ] type for static expressions
- [ ] fix ambiguities in what's a tuple vs. function call
# Bugs
- [ ] `{i:nat;j:int}`
- [ ] break on eq rather than `(` in `vtypedef parser(a : vt@ype+) = @{ modify = cstream -<lincloptr1> (cstream, a) }`
- [ ] `Arg` should include proofs, e.g. `ptr_get0<a> (pf1 | p1)`.
- [ ] macdef
- [ ] `assume`
- [ ] `castfn`
- [ ] `propdef`
- [ ] `abstype`
- [ ] `absvtype`
- [ ] `view`
- [ ] `prfn`
- [ ] `tkindef` / `typekindef`?
- [ ] `prfun`
- [ ] `propdef`
- [ ] `stacst`
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
