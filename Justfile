clean:
    sn c .
    rm -rf tags *.c

polyglot:
    @poly . -e data/

bench:
    bench "atsfmt test/data/polyglot.dats"

manpages:
    pandoc man/MANPAGE.md -s -t man -o man/atsfmt.1

install:
    @cabal new-build
    @cp $(fd 'atsfmt$' -IH dist-newstyle | tail -n1) ~/.local/bin
    @cp man/atsfmt.1 ~/.local/share/man/man1

ci: test
    cabal new-build
    cabal new-test
    hlint src app bench test
    tomlcheck --file .atsfmt.toml
    stack build --test --bench --no-run-tests --no-run-benchmarks
    weeder

test:
    @patscc -DATS_MEMALLOC_LIBC test/data/polyglot.dats -cleanaft -o pre &> /dev/null
    atsfmt test/data/polyglot.dats > tmp.dats
    @patscc -DATS_MEMALLOC_LIBC tmp.dats -cleanaft -o post &> /dev/null
    diff <(./pre) <(./post)
    @rm -f pre post *.c tmp.dats

size:
    @sn d $(fd 'atsfmt$' -IH dist-newstyle | tail -n1)
