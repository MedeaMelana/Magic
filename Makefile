run:
	ghci -Wall -iMagic/src:Magic-Cards/src:tests Main

clean:
	find . \( -name '*.o' -or -name '*.hi' \) -exec rm {} \;
	rm -rf Magic/dist Magic-Cards/dist

compile:
	ghc --make -Wall -iMagic:Magic-Cards:tests Main
