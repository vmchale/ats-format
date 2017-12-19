# ats-formatter

[![Build Status](https://travis-ci.org/vmchale/ats-format.svg?branch=master)](https://travis-ci.org/vmchale/ats-format)

This is a code formatter for [ATS](http://www.ats-lang.org/). It is
a work-in-progress, but it can handle a subset of the language already.

If you find something that's not listed in `TODO.md` feel free to open
an issue.

## Configuration

`atsfmt` is configured with the `.atsfmt.toml` file. As an example:

```toml
ribbon = 0.6 # maximum ribbon fraction
width = 120 # maximum width
clang-format = false # call clang-format on inline code
```

## Installation

### Binary Releases

The [releases](https://github.com/vmchale/ats-format/releases) page has binary
releases for common platforms.

### Compilation from Source

To install, first install [GHC](https://www.haskell.org/ghc/download.html), then
[cabal](https://www.haskell.org/cabal/download.html). Then

```bash
 $ cabal update
 $ cabal install ats-format
```

## License

All code except `test/data/left-pad.dats` is license under the BSD3 license.
