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
BIN_DIR = ./bin
DILLIG_INTERPRETER = ./ref-interpreter

RUN_TIMEOUT = 10

.DEFAULT_GOAL=all

all: $(LEXER) $(PARSER) $(L_INTERPRETER)

$(LEXER_OUT): src/Lexer.x Makefile
	cd src;	alex Lexer.x

$(LEXER): src/Lexer.x $(LEXER_OUT) Makefile | $(BIN_DIR)
	cd src; ghc -o lexer LexMain.hs
	mv src/lexer $(LEXER)

$(PARSER_OUT): $(LEXER_OUT) Makefile src/Grammar.hs src/Parser.y
	cd src; happy Parser.y

$(PARSER): $(PARSER_OUT) Makefile | $(BIN_DIR)
	cd src; ghc -o parser ParseMain.hs
	mv src/parser $(PARSER)

$(L_INTERPRETER): $(LEXER_OUT) $(PARSER_OUT) Makefile src/Evaluator.hs | $(BIN_DIR)
	cd src; ghc -o hazkl Evaluator.hs
	mv src/hazkl $(L_INTERPRETER)

clean: cleansrc cleanbin cleantests

cleansrc:
	cd src; rm -f *.o *.hi lexer Lexer.hs parser Parser.hs hazkl

cleanbin: | $(BIN_DIR)
	cd bin; rm -f *

test : $(RESULTS)

$(OUTS) : %.out : .FORCE %.L $(L_INTERPRETER)
	((timeout $(RUN_TIMEOUT) $(L_INTERPRETER) $*.L 2>&1 < $$(test -f $*.in && echo $*.in || echo /dev/null) || true) | egrep -v "(Run-time error|parse error|syntax error)" > $*.out) || true

$(DILLIG) : %.dillig : .FORCE
	((timeout $(RUN_TIMEOUT) ${DILLIG_INTERPRETER} $*.L 2>&1 < $$(test -f $*.in && echo $*.in || echo /dev/null) || true) | egrep -v "(Run-time error|parse error|syntax error)" > $*.dillig) || true

$(DIFFS) : %.diff : .FORCE %.dillig %.out
	diff -u $*.out $*.dillig > $*.diff 2>&1 || true

$(RESULTS) : %.result : .FORCE %.diff
	@echo -n "--- $* ... "
	@(test -s $*.diff && (echo "fail ---")) || (echo "pass ---")

cleantests:
	cd $(TEST_DIR); rm -f *.result *.dillig *.out *.diff

.FORCE:
