
DIST=dist
CBD=cabal-dev

default: clean build rebuild

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

demo:
	cd example/ && runghc -package-conf=../cabal-dev/packages-7.4.1.conf snap.hs -b 127.0.0.1 -p 8888

