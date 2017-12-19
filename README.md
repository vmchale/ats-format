# ats-formatter

This is a code formatter for [ATS](http://www.ats-lang.org/). It is
a work-in-progress, but it can handle a good chunk of the language already.

If you find something that's not listed in `TODO.md` feel free to open
an issue.

## Configuration

`atsfmt` is configured with the `.atsfmt.toml` file. You can generate a default
config file with

```bash
 $ atsfmt --default-config
```

## Installation

To install, first install [GHC](https://www.haskell.org/ghc/download.html), then
[cabal](https://www.haskell.org/cabal/download.html). Then

```bash
 $ cabal update
 $ cabal install ats-format
```

## License

All code except `test/data/left-pad.dats` is license under the BSD3 license.
