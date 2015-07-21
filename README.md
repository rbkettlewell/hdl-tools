# Hardware-Descriptive-Language (HDL) Development Tools

The first tool we are building will provide some tools to visualize the entity-dependencies
of HDL projects (initially we are only going to be targeting VHDL).


## 0 - Required Tools and build procedure
i) cabal install alex
ii) alex Token.x generates Token.hs which contains the lexing functions
iii) ghc --make Token.hs
iv) ghc --make VhdlParser.hs
v) run with the command: ./VhdlParser --path="[VHDL FOLDER PATH]"


## 1 - Develop Test Suite

We want to develop some simple tests for our core parsing algorithm; this will
help identify if we have a good data structure for our output, as well as
identify side cases that our parser needs to handle (e.g. various forms of
malformed VHDL files or rarely used VHDL syntax).

Deliverables:

- Get three examples of properly formed VHDL projects of varying complexity
- Get an example of a VHDL project with invalid syntax
- Write out JSON files representing the "correct output" for these projects
- Write a test-runner that can run all these tests

## 2 - Write the Parser

Alex first implementation in Tokens.x. Right now the file is basically just tokenizing any non-whitespace character and providing a alexScanTokens function.

## 3 - Visualization

TBD
