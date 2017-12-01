lex: src/Lexer.x Makefile src/Token.hs
	cd src;	alex Lexer.x

lexer: lex src/Lexer.x src/Lexer.hs Makefile
	cd src; ghc -o lexer LexMain.hs

clean:
	cd src; rm -f *.o *.hi lexer Lexer.hs
