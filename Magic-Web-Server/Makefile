run: build
	./magic-web-server

build:
	ghc --make -Wall -isrc:../Magic/src:../Magic-Cards/src -o magic-web-server Main
	# codesign -s "Martijn van Steenbergen" -f magic-web-server

clean:
	rm -rf dist magic-web-server
