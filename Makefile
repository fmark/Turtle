all: bin/turtle

bin/turtle : src/Tokenize.hs src/Parse.hs src/PrettyPrint.hs src/AbsSyn.hs src/Translate.hs src/Desugar.hs
	mkdir -p bin
	ghc -isrc -o bin/turtle --make src/Tokenize.hs src/Parse.hs src/PrettyPrint.hs src/AbsSyn.hs src/Translate.hs src/Desugar.hs

zip : src/Parse.y src/Tokenize.x src/PrettyPrint.hs src/AbsSyn.hs src/Translate.hs src/Desugar.hs Makefile README runTests.sh
	tar -zcvf turtle.tar.gz src/Parse.y src/Tokenize.x src/PrettyPrint.hs src/AbsSyn.hs src/Translate.hs src/Desugar.hs Makefile README runTests.sh tests/*.t tests/*.p tests/*.d tools/PDPlot-Haskell/PDPlot.hs tools/PDPlot-Haskell/Graphics.lhs

test : tools/PDPlot-Haskell/PDPlot bin/turtle
	./runTests.sh

tags : src/Tokenize.hs src/Parse.hs src/PrettyPrint.hs src/AbsSyn.hs src/Translate.hs src/Desugar/hs
	hasktags -e src/PrettyPrint.hs src/AbsSyn.hs src/Translate.hs src/Parse.y src/Tokenize.x src/Desugar.hs

src/Tokenize.hs : src/Tokenize.x
	alex -g src/Tokenize.x

src/Parse.hs : src/Parse.y
	happy -gac src/Parse.y

tools/PDPlot-Haskell/PDPlot : tools/PDPlot-Haskell/PDPlot.hs tools/PDPlot-Haskell/Graphics.lhs
	ghc --make tools/PDPlot-Haskell/PDPlot.hs tools/PDPlot-Haskell/Graphics.lhs

clean :
	find . -name "*.o" -o -name "*.hi" -o -name "*~"  | xargs rm -f
	rm -f bin/turtle src/Tokenize.hs src/Parse.hs TAGS tools/PDPlot-Haskell/PDPlot
