# The Semantic Analyzer  
Trey Rubino -  
CPSC 425 -  
Dr. Schwesinger  

## Project Structure  
- `main.ml`: Entry point, loads AST, runs validations and type checking, and writes `.cl-type`  
- `env.ml`: Maintains parent map, method and attribute environments, and subtype helpers  
- `typecheck.ml`: Core type-checking engine implementing COOL’s static semantics  
- `validate.ml`: Validation checks including inheritance legality, duplicates, overrides, and scoping  
- `reader.ml`: Deserializers AST from `.cl-ast` file into memory  
- `writer.ml`: Emits `.cl-type` including class map, implementation map, parent map, and annotated AST  
- `ast.ml`: Shared type definitions for COOL programs and expressions (same as other phases)  
- `Makefile`: Build instructions for compiling with the OCaml toolchain  
- `build/`: Directory containing generated lexer artifacts and compiled output  
- `test/`: Test files displaying good and bad test cases  

## Overview  
The semantic analyzer phase extends the interpreter from syntactic analysis into static semantic enforcement.
It ensure that COOL programs respect the language's type system, inheritance constraints, and scoping rules before
they are eligible for execution. This phase reports errors such as refined base classes, inheritance from forbidden or
undefined classes, cycles in the class hierarchy, duplicate or illegal identifiers, invalid method overrides, undeclared
variables, and non conforming type in expressions and method bodies. By completing this stage, a program is guaranteed
to be type safe and ready for runtime execution.

## Design  
The analyzer constructs several global environments. The parent map tracks class inheritance and is checked for cycles 
and illegal bases. The method environment stores signatures for both built-in and user-defined methods, enforcing arity, 
parameter type, and return type compatibility with overrides. The attribute environment records class attributes, propagating
inherited fields and checking for redefinitions. Together these environments allowed for recursive type checking of expressions, 
supported by subtype reasoning and least upper bound computation for joins in conditional and case constructs. The type system 
distinguishes between `Class c` and `SELF_TYPE c`, with special handling to preserve static guarantees.

## Implementation  
The main function orchestrates semantic analysis by reading the input AST from the `.cl-ast` file, running validation passes,
seeding built-in classes, attributes, and methods, and then type-checking user-defined classes. Each expression is annotated with
its static type by the `type_check` function, which recursively enforces typing rules for arithmetic, conditionals, dispatches, 
lets, cases, and object creation. Special restrictions on `self`, `SELF_TYPE`, and primitive types are enforced during analysis. 
Validation routines check structural correctness prior to type checking, ensuring that all classes, methods, and attributes conform
to COOL's rules. On success, the system produces `.cl-type` file containing the class map, implementation map, parent map, and annotated
AST, forming the input to later runtime phases.

## Testing
Injecting scripts with invalid types. Reading a COOL program by its lexical structure, matching on random type and identifiers swapper there position. We need cases that parse correctly, so running the the reference compiler over the generated "injected" scripts to confirm functionality and then delta debugging of how the hand-rolled interpreter handles these cases. Programs were used as data, and the reference compiler was analyze extracting string literals into a large corpus and then analyzed further to extract specific error cases or error messages.

## References  
[1] “The Cool Reference Manual,” Alex Aiken (et al.), Stanford University, The COOL Language Project, Jan. 2011. 
[Online]. Available: https://theory.stanford.edu/~aiken/software/cool/cool-manual.pdf

[2] “Type system — Type checking,” Wikipedia: The Free Encyclopedia, Sep. 25, 2025. 
[Online]. Available: https://en.wikipedia.org/wiki/Type_system#Type_checking

[3] “Video Guide - PA4t” YouTube, uploaded by westleyweimer6512, 
[Online]. Available: https://www.youtube.com/watch?v=2CMhLddBt1M&t=2s

[4] “Video Guide - PA4c” YouTube, uploaded by westleyweimer6512, 
[Online]. Available: https://www.youtube.com/watch?v=Wa9zMygcv_M

[5] “Video Guide - PA4” YouTube, uploaded by westleyweimer6512, 
[Online]. Available: https://www.youtube.com/watch?v=Oxpgrkmsxhg

[3] A. V. Aho, M. S. Lam, R. Sethi, and J. D. Ullman, Compilers: Principles, Techniques, 
and Tools, 2nd ed., ch. 6, “Semantics, Code Generation, and Optimization,” Pearson/Addison-Wesley, 2006.