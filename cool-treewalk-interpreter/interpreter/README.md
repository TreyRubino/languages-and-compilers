# The Operational Semantics 
Trey Rubino - 
CPSC 425 -
Dr. Schwesinger

## Project Structure
- `main.ml`: Entry point, loads runtime environment and executes Main.main
- `runtime.ml`: Defines runtime objects, values, scopes, and helpers
- `eval.ml`: Core evaluation engine implementing COOL's operational semantics
- `reader.ml`: Deserializers typed AST and runtime maps from `.cl-type` file
- `ast.ml`: Shared type definitions for COOL programs and expressions (same as the checkers)
- `Makefile`: Build instructions for compiling with the OCaml toolchain  
- `build/`: Directory containing generated lexer artifacts and compiled output  
- `test/`: Test files displaying good and bad test cases

## Overview
The operational semantics phase extends the interpreter from syntax and type analysis (static) into executable (dynamic)
behavior. It defines how COOL expression and statements evaluate at runtime, mapping abstract syntax to concrete effects and
values. This phase supports object creation, attribute initialization, method dispatch, expression evaluation, and control
structures. Runtime errors detected here include division by zero, unbound identifiers, invalid dispatches, and type
incompatible operations.

## Design
The runtime environment stores class attributes, method implementations, and inheritance relations. Objects are
modeled with field tables and defaults for uninitialized attributes. A value domain is defined with `Int`, `Bool`,
`String`, objects, and `Void`. Scoping is handled lexically with activation records pushed and popped as blocks,
lets, and methods execute. The evaluator implements COOL's semantics recursively evaluating expressions, dispatching
methods (both user defined and internal), and applying runtime checks.

## Implementation
The `eval` function is the core engine, pattern matching on expression kinds and producing runtime values. Built in
methods for `Object`, `IO`, and `String` are provided internally, while user methods execute within new scopes with bound
formals. Attribute initialization is applied in ancestor to descendant order as per the COOL reference manual. Dynamic,
static, and self dispatch fully supported, with `SELF_TYPE` handled by resolving to the runtime class of `self`. On error, 
the system prints a location tagged runtime message and exits, ensuring deterministic and auditable behavior.

## Testing
Functionality for object aliasing, SELF_TYPE dynamic resolution, and static and dynamic dispatching to showcase 
the runtime, and managed heap is structurally and functionality verified. Delta debugging was used here in further edge case debugging since it (delta debugging) can over generalize on the test cases and cause some programs to become lexical, syntactically, or semantically invalid. In this phase we want well structured and typed programs that would create a runtime exception or is probably handling cases like object aliasing. A few other testing cases verify dispatch on void, heap management, no match case branch, and all built-ins handle their interactions according the the COOL Reference Manual.

## References
