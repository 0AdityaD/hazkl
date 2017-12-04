.DEFAULT_GOAL=lexer

init:
	mkdir -p bin

lex: init src/Lexer.x Makefile src/Token.hs
	cd src;	alex Lexer.x

lexer: init lex src/Lexer.x src/Lexer.hs Makefile
	cd src; ghc -o lexer LexMain.hs
	mv src/lexer bin
	make -s cleansrc

parse: init lex Makefile src/Grammar.hs src/Parser.y
	cd src; happy Parser.y

parser: init parse Makefile
	cd src; ghc -o parser ParseMain.hs
	mv src/parser bin
	make -s cleansrc

hazkl: init parse lex Makefile src/Evaluator.hs
	cd src; ghc -o hazkl Evaluator.hs
	mv src/hazkl bin
	make -s cleansrc

clean: cleansrc cleanbin

cleansrc:
	cd src; rm -f *.o *.hi lexer Lexer.hs parser Parser.hs hazkl

cleanbin: init
	cd bin; rm -f *
