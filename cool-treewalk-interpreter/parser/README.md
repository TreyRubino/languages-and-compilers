# The Parser
Trey Rubino - 
CPSC 425 -
Dr. Schwesinger

## Project Structure
- `main.mly`: OCamlyacc grammar specification for COOL
- `Makefile`: Build instructions for compiling and testing with the OCaml toolchain  
- `build/`: Directory containing generated parser artifacts and compiled output  
- `test/`: Test files displaying good and bad test cases
  
## Overview
The Parser phase takes the stream of tokens produced by the lexer and builds an abstract syntax tree (AST)
that captures the structure of COOL programs. Parsing enforces the language's grammar rules and ensures
that token sequences form syntactically valid programs. Errors detected here include unexpected or misplaced 
tokens, missing delimiters, and violation of COOL's grammar. This phase is critical for bridging lexical
analysis and later semantic checks.

## Design
The Parser is implemented with OCamlyacc and structured as a context free grammar (CFG) over COOL's syntax. It
defines productions for classes, features, formals, expression, and control structures, each mapped to a 
corresponding AST node. Operator precedence and associativity are specified to disambiguate expressions such
as arithmetic and comparisons. The grammar closely follows the COOL reference manual to preserve 
compatibility with the language definition.

## Implementation
The actual grammar is implemented in `142` lines, just above the reference compiler's `116` line implementation.
AST node types are defined in the parser specification, with OCaml constructors representing classes,
methods, attributes, and expressions. Grammar rules use line numbered tokens to attach location information
for error reporting. The parser integrates with the lexer output by deserializing token streams and
feeding them into OCamlyacc's parsing engine. On syntax errors, the parser reports the offending token and line number,
then exits cleanly. Successfully parsed programs are serialized as `.cl-ast` files for later semantic analysis.

## References
