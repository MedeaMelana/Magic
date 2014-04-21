run:
	cabal build
	dist/build/mtg/mtg

configure:
	cabal configure

docs: configure
	cabal haddock

opendocs: docs
	open dist/doc/html/Magic-Cards/index.html
