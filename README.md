# Hardware-Descriptive-Language (HDL) Development Tools

The first tool we are building will provide some tools to visualize the entity-dependencies
of HDL projects (initially we are only going to be targeting VHDL).


## 0 - Required Tools and build procedure
i) Cabal install alex
ii) alex Token.x generates Token.hs which contains the lexing functions
iii) ghc --make VhdlParser.hs
iv) Run with the command:
    ./VhdlParser --path=[VHDL FOLDER PATH] --topLevel=[Top Level Filename (omit extension)]

Example cmd line output for FIR project:
[bkettlew@vbox hdl-tools]$ ./VhdlParser --path=/home/bkettlew/sandbox/hdl-tools/FIR/ --topLevel=FIR_topEntity_0
FIR_topEntity_0
|
+- FIR_zdwwindowzm_1
|
`- FIR_dotp_2
   |
   +- FIR_satMult_3
   |
   `- FIR_dotp1_4
      |
      `- FIR_satPlus_5

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
