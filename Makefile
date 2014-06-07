SANDBOX=.cabal-sandbox
DIST=dist
CBD=cabal
DEMO=$(SANDBOX)/bin/demo

default: clean build rebuild

init:
	test -e cabal.sandbox.config || cabal sandbox init
	cabal install --only-dependencies --job=2

clean:
	$(CBD) clean

conf:
	$(CBD) configure

build: conf
	$(CBD)  build
	hlint src/
	stylish-haskell -i src/Snap/Snaplet/*.hs

rebuild: clean build

install: build
	$(CBD)  install

reinstall: clean install
	$(CBD)  haddock
	$(CBD)  sdist

demo: install
	$(DEMO) -p 8811
