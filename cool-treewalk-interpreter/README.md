# COOL Tree-walk Interpreter
Trey Rubino - 
CPSC 425 -
Dr. Schwesinger

## Project Structure
- `checker/`: Implements static analysis and semantic checks. Includes class validation, inheritance 
              graph checks, method/attribute type environments, and expression type checking.
- `interpreter/`: Tree-walk interpreter that evaluates the typed AST at runtime. Provides operational 
                  semantics for COOL, handling object creation, method dispatch, control flow and expression evaluation.
- `lexer/`: Lexical analysis phase. Contains OCamllex scanner definition that tokenizes COOL source files 
            into a stream of tokens.
- `parser/`: Parsing phase. Uses OCamlyacc to transform token streams into abstract syntax tree (ASTs) 
             according to COOL grammar.
- `tests/`: Contains library of COOL programs to test the interpreter on. Covers a wide range of 
            functionality for all parts of this project. Specific unit tests are found in the individual directories.
- `cool-manual.pdf`: The official COOL language specification and reference manual used for grammar, 
                     semantics, and test conformance.

## Overview
This project implements a tree-walk interpreter for the COOL (Classroom Object-Oriented Language). The interpreter 
is written in OCaml and follows the traditional compiler pipeline, beginning with lexical analysis and parsing, 
progressing through semantic analysis, and culminating in interpretation of programs directly from their abstract 
syntax trees. Each stage is structured as its own subsystem, ensuring separation of concerns while preserving a coherent
flow from raw source code to runtime execution.  

The interpreter faithfully implements the COOL specification, including lexical rules, grammar, type system, and 
operational semantics. Programs are validated through the semantic checker before execution, ensuring correctness 
and conformance to the language’s constraints. At runtime, the interpreter evaluates expressions, performs method dispatch, 
instantiates objects, initializes attributes, and applies control structures as defined by the language semantics. The
design emphasizes clarity, modularity, and extensibility so that the system can serve both as a teaching tool and as a 
foundation for further exploration of programming language theory and compiler construction. 

## Design
The interpreter is organized around a modular architecture that mirrors the structure of the compiler pipeline. 
The lexer and parser are responsible for transforming raw source code into an abstract syntax tree, which provides a 
structured representation of COOL programs. The checker then traverses this tree to enforce type safety, validate inheritance 
hierarchies, and construct the necessary environments for methods and attributes. By the time execution begins, all static errors 
have been identified and rejected, ensuring that the interpreter only evaluates well-formed programs.  

Execution is handled by the interpreter, which implements COOL’s operational semantics through a direct traversal of the 
typed abstract syntax tree. Each expression node is associated with a concrete evaluation rule, producing runtime values 
or triggering effects such as method dispatch and object creation. The runtime environment is structured around scopes and object
tables, enabling correct handling of variables, dynamic dispatch, and default initialization. Control constructs such as conditionals 
and loops are reduced directly in the interpreter, reflecting their semantics as described in the language manual.  

This design balances readability and rigor. Each phase is isolated in its own module so that changes to the lexer, parser, 
or checker do not disrupt the interpreter’s logic. At the same time, shared type definitions ensure consistency across the system. 
The resulting structure is both pedagogical and extensible, allowing the project to demonstrate core principles of programming
language implementation while remaining a foundation for further experimentation, such as bytecode translation or garbage collection.

## Implementation
The interpreter is implemented in OCaml, chosen for its strong type system, pattern matching, and functional programming style, 
all of which are well suited to compiler and interpreter construction. OCaml’s type inference and algebraic data types make 
it straightforward to model abstract syntax trees, environments, and runtime values with precision and safety. Pattern
matching provides a direct and expressive way to implement evaluation rules, allowing the code to closely mirror the formal 
operational semantics of the COOL language.  

The build system is managed through `make`, with `ocamllex` and `ocamlyacc` used for lexical and syntactic analysis. 
These tools integrate cleanly with the OCaml ecosystem and support rapid iteration on the language frontend. 
Semantic checking is performed using custom modules that construct and traverse environments, ensuring that class hierarchies, 
method definitions, and type constraints are validated before execution. Once the AST has been type-annotated, the interpreter 
traverses it in a recursive descent style, applying evaluation rules node by node.  

At runtime, the system maintains a structured environment that maps variable names to values and provides mechanisms for 
dynamic dispatch, object instantiation, and attribute initialization. Scoping rules and inheritance are enforced by walking 
the runtime environment and parent tables, ensuring consistency with the COOL specification. Error handling is incorporated into the
evaluation process to detect invalid operations such as division by zero or dispatch on void, mirroring the runtime errors described 
in the language manual.  

OCaml was chosen not only for technical advantages but also for its alignment with the goals of the project. As a language used 
heavily in research and teaching of programming language theory, OCaml provides the rigor needed for correctness, the 
flexibility needed for experimentation, and the performance found in the industry. This makes it an ideal platform for implementing a 
pedagogical interpreter that is faithful to the COOL specification while remaining extensible for future enhancements.

## Build & Run Instructions
This project is intended for Unix based environments (macOS or Linux).  
Windows is not supported directly, if you’re on Windows, use **WSL** (Windows Subsystem for Linux) or follow a separate Windows OCaml 
installation guide (not covered here).

### macOS Prerequisite
Install [Homebrew](https://brew.sh) if you haven’t already.

---

### 1. Install `opam` (OCaml’s package manager)
```
sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
```
---

### 2. Verify installation  
You should be on version **2.x**.
```
opam --version
```
---

### 3. Initialize `opam`
```
opam init
```

- When prompted to update your shell profile, confirm with `y`.  
- After initialization, reload your shell config (`~/.zshrc` or `~/.bash_profile` depending on your shell).
```
# For zsh
source ~/.zshrc

# For bash
source ~/.bash_profile
```
---

### 4. Install and switch to OCaml 5.3  
Check your current OCaml version.  
```
ocaml --version
```

If not 5.3, update, switch, and reload environment.  
```
opam update
opam switch 5.3.0
eval $(opam env)
```

Verify that you are on **5.3.0**.
``` 
ocaml --version
```
---

### 5. Install dependencies  
Packages required: `ocamlfind`, `ounit`, `utop`, `dune`, `qcheck`.
```
opam install ocamlfind ounit utop dune qcheck
```
---

### 6. Build and run
From the project root:  
- Build the project with `make`.  
- Interpret a COOL source file with `cooli <source-file.cl>`.

## An Engineer's Note
I want to personally thank Dr. Dylan Schwesinger at Kutztown University. He not only served as the overseeing professor for this project,
as well as the custom bytecode interpreter and garbage collection extension I am currently developing, but he has also been my advisor
and the single most influential figure in introducing me to the world of programming language theory and compiler construction.  

I first approached him after class in Network Programming to talk about my career goals. At the time, I believed the “magic” of computer
science was found in firmware and device drivers, and I told him that was what I wanted to pursue. His response was simple but
transformative: *“Why would you want to do that? There’s so much more out there.”* I took that as an invitation toward programming
languages and compilers, and within weeks I was enrolled in an independent study in Compiler Design with him. Not long after, he gifted me
a copy of the second edition of the Dragon Book, which became my entry point into compiler design and a constant reference throughout
this project.  

Dr. Schwesinger, thank you for opening my eyes to what truly defines the "magic" of computers and puts the "science" in computer science. 
Your mentorship has reshaped not only this project, but my entire perspective on the field.
