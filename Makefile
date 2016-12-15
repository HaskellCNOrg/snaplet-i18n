CBD=stack

default: rebuild

clean:
	$(CBD) clean

build:
	$(CBD) build

style:
	hlint src/ example/
	stylish-haskell -i src/Snap/Snaplet/*.hs example/*.hs

ci: build

demo: build
	$(CBD) exec demo -- --port 8811
