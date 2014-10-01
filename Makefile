.PHONY: build test install clean

build:
	cabal configure --enable-tests && cabal build

test: build
	cabal test || true

install:
	cabal install

clean:
	cabal clean
