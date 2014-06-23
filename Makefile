.PHONY: all build clean configure haddock install repl

all: install configure build haddock

build:
	cabal build

clean:
	cabal clean
	cabal sandbox delete

configure:
	cabal configure

haddock:
	cabal haddock
	# dist/doc/html/scurry/index.html

install:
	cabal sandbox init
	cabal install --only-dependencies

repl:
	cabal repl lib:scurry
