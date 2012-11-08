run:
	ghci -Wall Magic.Engine Magic.M12 Magic.BasicLands

configure:
	cabal configure

docs: configure
	cabal haddock

opendocs: docs
	open dist/doc/html/Magic/index.html