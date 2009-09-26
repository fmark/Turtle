all : src/Tokenize.hs
	ghc -isrc -o bin/turtle --make src/Tokenize.hs

src/Tokenize.hs : src/Tokenize.x
	alex src/Tokenize.x

clean :
	find . -name "*.o" -o -name "*.hi" -o -name "*~"  | xargs rm -f
	rm -f bin/turtle src/Tokenize.hs
