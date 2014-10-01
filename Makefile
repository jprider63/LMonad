.PHONY: build test clean

build:
	cabal configure --enable-tests && cabal build

test: build
	cabal test || true

clean:
	cabal clean
