
DIST=dist
CBD=cabal-dev

default: clean build rebuild

clean:
	rm -rf $(DIST)
	rm -rf ./cabal-dev/lib/snaplet-i18n*
	rm -rf ./cabal-dev/packages/snaplet-i18n*
	rm -f ./cabal-dev/packages-7.4.1.conf/snaplet-i18n-*


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

demo:
	cd example/ && runghc -package-conf=../cabal-dev/packages-7.4.1.conf snap.hs -b 127.0.0.1 -p 8888
