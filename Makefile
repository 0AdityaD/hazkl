.DEFAULT_GOAL=lexer

init:
	mkdir -p bin

lex: init src/Lexer.x Makefile src/Token.hs
	cd src;	alex Lexer.x

lexer: init lex src/Lexer.x src/Lexer.hs Makefile
	cd src; ghc -o lexer LexMain.hs
	mv src/lexer bin
	make -s cleansrc

clean: cleansrc cleanbin

cleansrc:
	cd src; rm -f *.o *.hi lexer Lexer.hs

cleanbin: init
	cd bin; rm -f *
