.PHONY: all build clean configure install

all: install configure build

build:
	cabal build

clean:
	cabal clean
	cabal sandbox delete

configure:
	cabal configure

install:
	cabal sandbox init
	cabal install --only-dependencies
