run:
	ghci -Wall Engine M12 BasicLands

docs:
	cabal haddock

opendocs: docs
	open dist/doc/html/Magic/index.html