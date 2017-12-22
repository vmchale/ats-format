# ats-formatter

[![Build Status](https://travis-ci.org/vmchale/ats-format.svg?branch=master)](https://travis-ci.org/vmchale/ats-format)

<img alt="Screenshot of sample results" src=https://github.com/vmchale/ats-format/raw/master/atsfmt.png>
<img alt="Screenshot of sample results" src=https://github.com/vmchale/ats-format/raw/master/atsfmt2.png>

This is a code formatter for [ATS](http://www.ats-lang.org/). It is
a work-in-progress, but it can handle a subset of the language already.
Currently it is only tested with ATS 0.3.8.

If you find something that's not listed in `TODO.md` feel free to open
an issue. The pretty-printer is a bit anemic, so you're welcome to submit code
samples where it produces bad output.

The formatter is pleasantly fast, formatting a 1500 line file in <20ms.

## Configuration

`atsfmt` is configured with the `.atsfmt.toml` file. You can generate a default
configuration with

```bash
 $ atsfmt --default-config
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

All code except `test/data/left-pad.dats` is licensed under the BSD3 license.
