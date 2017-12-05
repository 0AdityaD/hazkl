.DEFAULT_GOAL=all

all:
	make --no-print-directory lexer
	make --no-print-directory parser
	make --no-print-directory hazkl

init:
	mkdir -p bin

lex: init src/Lexer.x Makefile src/Token.hs
	cd src;	alex Lexer.x

lexer: init lex src/Lexer.x src/Lexer.hs Makefile
	cd src; ghc -o lexer LexMain.hs
	mv src/lexer bin
	make --no-print-directory cleansrc

parse: init lex Makefile src/Grammar.hs src/Parser.y
	cd src; happy Parser.y

parser: init parse Makefile
	cd src; ghc -o parser ParseMain.hs
	mv src/parser bin
	make --no-print-directory cleansrc

hazkl: init parse lex Makefile src/Evaluator.hs
	cd src; ghc -o hazkl Evaluator.hs
	mv src/hazkl bin
	make --no-print-directory cleansrc

clean: cleansrc cleanbin

cleansrc:
	cd src; rm -f *.o *.hi lexer Lexer.hs parser Parser.hs hazkl

cleanbin: init
	cd bin; rm -f *
