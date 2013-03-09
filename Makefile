run:
	ghci -Wall -i.:tests Main

configure:
	cabal configure

docs: configure
	cabal haddock

opendocs: docs
	open dist/doc/html/Magic/index.html

clean:
	find . \( -name '*.o' -or -name '*.hi' \) -exec rm {} \;

compile:
	ghc --make -Wall -i.:tests Main
