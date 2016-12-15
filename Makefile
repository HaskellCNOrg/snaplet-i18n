CBD=stack

default: rebuild

clean:
	$(CBD) clean

build:
	hlint src/ example/
	stylish-haskell -i src/Snap/Snaplet/*.hs example/*.hs
	$(CBD) build

rebuild: clean build

demo: build
	$(CBD) exec demo -- --port 8811
