CC = g++
CFLAGS = -g -Wall -std=c++0x
INC=-. ./ast
INC_PARAMS=$(foreach d, $(INC), -I$d)

TEST_DIR = ./tests/hazkl

TESTS := $(sort $(wildcard ./${TEST_DIR}/*.L ))
OUTS := $(patsubst %.L, %.out, $(TESTS))
DILLIG := $(patsubst %.L, %.dillig, $(TESTS))
DIFFS := $(patsubst %.L, %.diff, $(TESTS))
RESULTS := $(patsubst %.L, %.result, $(TESTS))

L_INTERPRETER = ./bin/hazkl
DILLIG_INTERPRETER = ./ref-interpreter

.DEFAULT_GOAL=all

all: lexer parser hazkl

init:
	mkdir -p bin

lex: init src/Lexer.x Makefile
	cd src;	alex Lexer.x

lexer: init lex src/Lexer.x src/Lexer.hs Makefile
	cd src; ghc -o lexer LexMain.hs
	mv src/lexer bin

parse: init lex Makefile src/Grammar.hs src/Parser.y
	cd src; happy Parser.y

parser: init parse Makefile
	cd src; ghc -o parser ParseMain.hs
	mv src/parser bin

hazkl: init parse lex Makefile src/Evaluator.hs
	cd src; ghc -o hazkl Evaluator.hs
	mv src/hazkl bin

clean: cleansrc cleanbin cleantests

cleansrc:
	cd src; rm -f *.o *.hi lexer Lexer.hs parser Parser.hs hazkl

cleanbin: init
	cd bin; rm -f *

$(OUTS) : %.out : .FORCE %.L
	-$(L_INTERPRETER) $*.L > $*.out 2>&1 || true

$(DILLIG) : %.dillig : .FORCE %.out
	-${DILLIG_INTERPRETER} $*.L > $*.dillig 2>&1

$(DIFFS) : %.diff : .FORCE %.dillig
	diff -u $*.out $*.dillig > $*.diff 2>&1 || true

$(RESULTS) : %.result : .FORCE %.diff
	@echo -n "--- $* ... "
	@(test -s $*.diff && (echo "fail ---")) || (echo "pass ---")

cleantests:
	cd $(TEST_DIR); rm -f *.result *.dillig *.out *.diff

.FORCE:
