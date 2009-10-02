all : src/Tokenize.hs src/Parse.hs src/PrettyPrint.hs src/AbsSyn.hs
	ghc -isrc -o bin/turtle --make src/Tokenize.hs src/Parse.hs src/PrettyPrint.hs src/AbsSyn.hs

src/Tokenize.hs : src/Tokenize.x
	alex -g src/Tokenize.x

src/Parse.hs : src/Parse.y
	happy -gac src/Parse.y

clean :
	find . -name "*.o" -o -name "*.hi" -o -name "*~"  | xargs rm -f
	rm -f bin/turtle src/Tokenize.hs src/Parse.hs
