# COOL Compiler & Virtual Machine  
Trey Rubino -  
CPSC 372 Independent Study -  
Dr. Schwesinger  

## Project Structure  
The repository is divided into a frontend and backend architecture, with core
definitions shared by all phases and a main driver that orchestrates the full
pipeline.

### Frontend  
- `lexer/`: Produces tokens from COOL source text, including identifiers,
  literals, keywords, operators, and error recovery for malformed input.  
- `parser/`: Implements the COOL grammar, constructs AST nodes with precise
  source locations, and reports syntax-level issues.  
- `checker/`: Performs semantic analysis including inheritance validation,
  environment construction, static type checking, and annotation of AST nodes
  with static type information.

### Backend  
- `codegen/`: Lowers the typed COOL program into IR form, constructs class and
  method layouts, synthesizes constructors, resolves dispatch, and emits the
  final bytecode instruction stream.  
- `vm/`: Executes the generated IR using a stack-based virtual machine,
  supporting object allocation, dispatch, primitive operations, and complete
  COOL runtime semantics.

### Shared Modules  
- `core/`: The compiler-wide foundation, defining the COOL AST, shared static
  types, bytecode ISA, intermediate representation structures, error-handling
  framework, and semantic environment types.

### Additional  
- `tests/`: Validation programs, IR dumps, and execution tests used during
  development.  
- `main.ml`: Entry point that wires together the frontend and backend, invokes
  each compilation stage, and runs or emits the generated IR.

## Overview  
This repository implements the full COOL toolchain—from raw source text to
executable bytecode and a functioning runtime. The pipeline follows a
traditional compiler structure: lexing and parsing produce the AST, semantic
analysis enforces correctness, code generation constructs a machine-executable
IR, and the VM interprets that IR deterministically. Each subsystem documents
its own behavior through a directory-level README; this file serves as a
top-level directory guide.

## Design  
The system adopts a clean, modular architecture. The frontend enforces all
static requirements, guaranteeing that only well-formed and well-typed programs
reach the backend. The backend assumes semantic correctness and produces a
compact IR along with deterministic bytecode for runtime execution. Shared core
modules centralize definitions for AST nodes, IR layout, bytecode instructions,
and the exception system to ensure consistency across all stages. The main
driver coordinates these components, forming a complete compiler-runtime loop.

## Implementation  
Compilation proceeds through strictly ordered phases: lexical analysis, parsing,
semantic validation, IR generation, and bytecode lowering. The VM loads the IR,
constructs the main object, invokes constructors, manages frames and runtime
stacks, and executes instructions until the entry method completes. Each module
interacts only through the shared structures defined under `core/`, ensuring a
consistent and maintainable implementation.
