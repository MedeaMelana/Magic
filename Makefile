run:
	ghci -Wall Engine M12

docs:
	cabal haddock

opendocs: docs
	open dist/doc/html/Magic/index.html