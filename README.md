
# Hazkl - an L interpreter in Haskell

`Hazkl`'s external dependencies are Alex (a Haskell equivalent of GNU flex)
and Happy (a Haskell equivalent of GNU `bison` or `Yacc`).

All packages were installed from Ubuntu's official \texttt{main} and \texttt{universe} repositories.
Below are the versions we used:

build-essential       12.1ubuntu2
ghc                   7.10.3-7
alex                  3.1.6-1
happy                 1.19.5-5

We installed these packages using the command:

```bash
sudo apt install build-essential ghc alex happy
```

To automate our testing, we adapted a Makefile from Prof. Gheith's CS 429H Computer Architecture class.

We use Pengxiang's updated reference interpreter to generate "correct" output, which the
Makefile then compares against ours to check for our implementation's validity.

This is done in the "make" target "test", which runs all tests in the test directory
and outputs whether we pass or fail each test.

Specifically, from the root project directory, the command we ran was:

```bash
make -s test
```

Our Makefile can produce three executables located in the bin directory of the project root

 * `lexer`
 * `parser`
 * `hazkl`

The first two are self-describing, and the last is our L interpreter.
The lexer and parser both take input through standard input.

This is an example of how to compile and run the lexer:

```bash
make bin/lexer
./bin/lexer < tests/hazkl/test78.L
```

Note that the output of the lexer does not match the output of the lexers from PA1 exactly.

This is an example of how to compile and run the parser:

```bash
make bin/parser
./bin/parser < tests/hazkl/test78.L
`````````````````````````````````

Note that the output of the parser does not match the output of the lexers from PA2 exactly.


The L interpreter requires an L script file as its sole argument. For example:

```bash
make bin/hazkl
./bin/hazkl tests/hazkl/test78.L
```
