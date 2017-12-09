CC = g++
CFLAGS = -g -Wall -std=c++0x
INC=-. ./ast
INC_PARAMS=$(foreach d, $(INC), -I$d)

TEST_DIR = ./tests/hazkl

TESTS := $(sort $(wildcard ./${TEST_DIR}/*.L ))
TEST_INS := $(patsubst %.L, %.in, $(TESTS))
OUTS := $(patsubst %.L, %.out, $(TESTS))
DILLIG := $(patsubst %.L, %.dillig, $(TESTS))
DIFFS := $(patsubst %.L, %.diff, $(TESTS))
RESULTS := $(patsubst %.L, %.result, $(TESTS))

LEXER_OUT = src/Lexer.hs
PARSER_OUT = src/Parser.hs

LEXER = ./bin/lexer
PARSER = ./bin/parser
L_INTERPRETER = ./bin/hazkl
DILLIG_INTERPRETER = ./ref-interpreter

RUN_TIMEOUT = 5

.DEFAULT_GOAL=all

all: lexer parser hazkl

init:
	mkdir -p bin

$(LEXER_OUT): init src/Lexer.x Makefile
	cd src;	alex Lexer.x

lexer: init src/Lexer.x $(LEXER_OUT) Makefile
	cd src; ghc -o lexer LexMain.hs
	mv src/lexer bin

$(PARSER_OUT): init $(LEXER_OUT) Makefile src/Grammar.hs src/Parser.y
	cd src; happy Parser.y

parser: init $(PARSER_OUT) Makefile
	cd src; ghc -o parser ParseMain.hs
	mv src/parser bin

hazkl: init $(LEXER_OUT) $(PARSER_OUT) Makefile src/Evaluator.hs
	cd src; ghc -o hazkl Evaluator.hs
	mv src/hazkl bin

clean: cleansrc cleanbin cleantests

cleansrc:
	cd src; rm -f *.o *.hi lexer Lexer.hs parser Parser.hs hazkl

cleanbin: init
	cd bin; rm -f *

test : $(RESULTS)

$(OUTS) : %.out : .FORCE %.L hazkl
	(timeout $(RUN_TIMEOUT) $(L_INTERPRETER) $*.L 2>&1 < $$(test -f $*.in && echo $*.in || echo /dev/null) || true) | grep -v "Run-time error" > $*.out

$(DILLIG) : %.dillig : .FORCE %.out
	(timeout $(RUN_TIMEOUT) ${DILLIG_INTERPRETER} $*.L 2>&1 < $$(test -f $*.in && echo $*.in || echo /dev/null) || true) | grep -v "Run-time error" > $*.dillig

$(DIFFS) : %.diff : .FORCE %.dillig
	diff -u $*.out $*.dillig > $*.diff 2>&1 || true

$(RESULTS) : %.result : .FORCE %.diff
	@echo -n "--- $* ... "
	@(test -s $*.diff && (echo "fail ---")) || (echo "pass ---")

cleantests:
	cd $(TEST_DIR); rm -f *.result *.dillig *.out *.diff

.FORCE:
