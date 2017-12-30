approve FILE:
    @atsfmt test/data/{{ FILE }} -o > test/data/$(echo {{ FILE }} | sed 's/\(dats\|sats\)/out/')

next:
    @export VERSION=$(ac ats-format.cabal | grep -P -o '\d+\.\d+\.\d+\.\d+' ats-format.cabal | head -n1 | awk -F. '{$NF+=1; print $0}' | sed 's/ /\./g') && echo $VERSION && sed -i "2s/[0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+/$VERSION/" ats-format.cabal
    git commit -am "next"
    sn c .

release:
    git tag "$(grep -P -o '\d+\.\d+\.\d+\.\d+' ats-format.cabal | head -n1)"
    git push origin --tags
    git tag -d "$(grep -P -o '\d+\.\d+\.\d+\.\d+' ats-format.cabal | head -n1)"
    git push origin master

upload:
    rm -rf dist/ .ghc.environment.*
    cabal sdist
    cabal upload --publish $(fd '\.tar\.gz$' -I)

diff FILE:
    @diff <(atsfmt test/data/{{ FILE }} -o) test/data/$(echo {{ FILE }} | sed 's/\(dats\|sats\)/out/') | ac -s

clean:
    sn c .
    rm -rf tags *.c

polyglot:
    @poly . -e data/

bench:
    bench "atsfmt test/data/polyglot.dats" "atsfmt test/data/left-pad.dats" "atsfmt ~/programming/ats/toml-parse/src/toml-parse.dats"

manpages:
    pandoc man/MANPAGE.md -s -t man -o man/atsfmt.1

install:
    @cabal new-build
    @cp $(fd 'atsfmt$' -IH dist-newstyle | tail -n1) ~/.local/bin
    @cp man/atsfmt.1 ~/.local/share/man/man1

ci: test
    cabal new-build
    cabal new-haddock
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
