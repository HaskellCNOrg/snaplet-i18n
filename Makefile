

HC=ghc

DIST=dist

default: build

clean:
	rm -rf $(DIST)

conf:
	cabal configure

build: conf
	cabal build
	hlint src/

rebuild: clean build

install: build
	cabal install

reinstall: clean install
	cabal haddock
	cabal sdist

test-demo:
	cd test/ && runghc snap.hs -b 127.0.0.1 -p 8888

