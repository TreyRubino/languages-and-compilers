# COOL Tree-walk Interpreter
Trey Rubino - 
CPSC 425 -
Dr. Schwesinger

## Project Structure
- `checker/`:
- `interpreter/`:
- `lexer/`:
- `parser/`:
- `tests/`:
- `cool-manual.pdf`: 

## Overview


## Design


## Implementation


## Build & Run Instructions
This project is intended for Unix based environments (macOS or Linux).  
Windows is not supported directly, if you’re on Windows, use **WSL** (Windows Subsystem for Linux) or follow a separate Windows OCaml installation guide (not covered here).

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
Verify that you are on **5.3.0**.
```
opam update
opam switch 5.3.0
eval $(opam env)
```

Then verify: 
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
