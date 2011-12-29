GHCFLAGS=-Wall -XNoCPP -fno-warn-name-shadowing -XHaskell98 -O2
HLINTFLAGS=-XHaskell98 -XNoCPP -i 'Use camelCase' -i 'Use String' -i 'Use head' -i 'Use string literal' -i 'Use list comprehension' --utf8
VERSION=0.2

.PHONY: all clean doc install shell

all: report.html doc dist/build/sock2stream/sock2stream dist/sock2stream-$(VERSION).tar.gz

install: dist/build/sock2stream/sock2stream
	cabal install

shell:
	ghci $(GHCFLAGS)

report.html: sock2stream.hs
	-hlint $(HLINTFLAGS) --report $^

doc: dist/doc/html/sock2stream/index.html README

README: sock2stream.cabal
	tail -n+$$(( `grep -n ^description: $^ | head -n1 | cut -d: -f1` + 1 )) $^ > .$@
	head -n+$$(( `grep -n ^$$ .$@ | head -n1 | cut -d: -f1` - 1 )) .$@ > $@
	-printf ',s/        //g\n,s/^.$$//g\n,s/\\\\\\//\\//g\nw\nq\n' | ed $@
	$(RM) .$@

dist/doc/html/sock2stream/index.html: dist/setup-config sock2stream.hs
	cabal haddock --hyperlink-source --executables

dist/setup-config: sock2stream.cabal
	cabal configure

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)
	$(RM) -r dist

dist/build/sock2stream/sock2stream: sock2stream.cabal dist/setup-config sock2stream.hs
	cabal build --ghc-options="$(GHCFLAGS)"

dist/sock2stream-$(VERSION).tar.gz: sock2stream.cabal dist/setup-config sock2stream.hs README
	cabal check
	cabal sdist
