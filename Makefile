.PHONY: all build clean configure format haddock install lenses repl test

all: install configure build test haddock

build:
	cabal build --jobs

clean:
	cabal clean
	cabal sandbox delete

configure:
	cabal configure --enable-tests

format:
	cabal format
	git ls-files '*.hs' | xargs -n 1 scan --inplace-modify --multiple-blanks=0 --template-haskell
	git ls-files '*.hs' | xargs stylish-haskell --inplace

haddock:
	cabal haddock

install:
	cabal sandbox init
	cabal install --allow-newer=base --enable-tests --jobs --only-dependencies

lenses:
	rm library/Strive/Lenses/Classes.hs
	rm library/Strive/Lenses/Instances.hs
	cabal exec runhaskell -- -Wall library/Strive/Lenses/lenses.hs

repl:
	cabal repl lib:strive

test:
	cabal test --jobs
