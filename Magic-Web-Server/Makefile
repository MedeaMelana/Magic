run: build
	./magic-web-server

build:
	cabal build
	codesign -s "Martijn van Steenbergen" -f dist/build/magic-web-server/magic-web-server
