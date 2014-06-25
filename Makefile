.PHONY: all build clean configure format haddock install lint repl

all: install configure build haddock

build:
	cabal build

clean:
	cabal clean
	cabal sandbox delete

configure:
	cabal configure

format:
	git ls-files '*.hs' | xargs -n 1 scan --inplace-modify
	git ls-files '*.hs' | xargs stylish-haskell --inplace

haddock:
	cabal haddock
	# dist/doc/html/strive/index.html

install:
	cabal sandbox init
	cabal install --only-dependencies

lint:
	git ls-files '*.hs' | xargs hlint

repl:
	cabal repl lib:strive
