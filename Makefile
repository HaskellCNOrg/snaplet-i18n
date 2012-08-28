
DIST=dist
CBD=cabal-dev

default: build

clean:
	rm -rf $(DIST)

conf:
	$(CBD) configure

build: conf
	$(CBD)  build
	hlint src/

rebuild: clean build

install: build
	$(CBD)  install

reinstall: clean install
	$(CBD)  haddock
	$(CBD)  sdist

test-demo:
	cd test/ && runghc snap.hs -b 127.0.0.1 -p 8888

