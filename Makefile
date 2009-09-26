all : src/Tokenize.hs src/Parse.hs
	ghc -isrc -o bin/turtle --make src/Tokenize.hs src/Parse.hs

src/Tokenize.hs : src/Tokenize.x
	alex src/Tokenize.x

src/Parse.hs : src/Parse.y
	happy src/Parse.y

clean :
	find . -name "*.o" -o -name "*.hi" -o -name "*~"  | xargs rm -f
	rm -f bin/turtle src/Tokenize.hs src/Parse.y
