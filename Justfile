clean:
    sn c .
    rm -rf tags *.c

polyglot:
    @poly . -e data/

bench:
    bench "atsfmt test/data/polyglot.dats" "atsfmt test/data/left-pad.dats"

manpages:
    pandoc man/MANPAGE.md -s -t man -o man/atsfmt.1

profile:
    @cabal new-build -p
    @cp $(fd 'atsfmt$' -IH dist-newstyle | tail -n1) ~/.local/bin
    @cp man/atsfmt.1 ~/.local/share/man/man1

install:
    @cabal new-build
    @cp $(fd 'atsfmt$' -IH dist-newstyle | tail -n1) ~/.local/bin
    @cp man/atsfmt.1 ~/.local/share/man/man1

ci: test
    cabal new-build
    cabal new-test
    hlint src app bench test
    tomlcheck --file .atsfmt.toml
    yamllint .travis.yml
    yamllint .hlint.yaml
    yamllint .stylish-haskell.yaml
    yamllint .yamllint
    stack build --test --bench --no-run-tests --no-run-benchmarks
    weeder

test:
    @rm -rf rm .ghc.environment.* polyglot
    @git clone https://github.com/vmchale/polyglot
    cd polyglot && atsfmt src/polyglot.dats -i
    cd polyglot && ./shake.hs
    @rm -rf polyglot

size:
    @sn d $(fd 'atsfmt$' -IH dist-newstyle | tail -n1)
