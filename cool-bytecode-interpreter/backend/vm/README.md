# The Virtual Machine  
Trey Rubino -  
CPSC 372 Independent Study -  
Dr. Schwesinger  

## Project Structure  
- `runtime.ml`: Defines the core runtime types used by the VM, including object
  representations, payload variants, values, frames, and the virtual machine
  state.  
- `alloc.ml`: Provides allocation routines for COOL objects, including
  constructor lookup, field initialization, boxing of primitive values, and
  SELF_TYPE allocation.  
- `stack.ml`: Implements the VM call stack and value stack, supporting frame
  creation, argument installation, local access, and stack-value operations.  
- `builtin.ml`: Handles all built-in COOL methods such as `abort`, `copy`,
  `type_name`, I/O routines, and string operations. Dispatches these before
  normal bytecode execution.  
- `exec.ml`: Core execution engine that interprets bytecode instructions,
  updates the program counter, manages frames, handles dispatch, applies
  arithmetic and boolean operations, and evaluates all runtime behaviors.  
- `vm.ml`: Entry point for constructing the VM, instantiating the main object,
  invoking its initializer, installing the entry method, and starting program
  execution.  
- `ir.ml`: Shared intermediate representation definitions used by the VM for
  method lookup, dispatch tables, and class/attribute layout.  

## Overview  
The Virtual Machine executes the intermediate representation produced by the
Code Generator, providing a concrete operational semantics for COOL programs.
Every class, attribute, method, bytecode instruction, and constructed object is
represented explicitly at runtime. The VM maintains its own heap, call frames,
value stack, and object model. It interprets bytecode step-by-step, performing
dynamic dispatch, evaluating expressions, invoking constructors, and carrying
out all primitive and built-in COOL operations. By implementing this phase, the
compiler pipeline becomes fully executable: once an IR program is emitted, the
VM provides an environment capable of running any well-typed COOL program to
completion.

## Design  
The VM is structured around three major components: the runtime object model,
the stack-based execution engine, and the allocation/built-in subsystems.  
Objects carry a class identifier, a vector of fields, and a payload representing
COOL’s primitive data. Methods are executed inside stack frames that contain a
program counter, a reference to the method’s IR descriptor, a local array for
formals and locals, and a self object. Execution follows the conventional
stack-machine model: values are pushed and popped from a runtime stack while
bytecode operations update frames, access attributes, perform arithmetic,
branch on conditions, evaluate control flow, and resolve method calls. Dynamic
dispatch is achieved through per-class dispatch tables populated by the Code
Generator, while static dispatch bypasses vtable lookup entirely. The VM also
provides boxing/unboxing behaviors for primitives, SELF_TYPE resolution during
allocation, and routing for COOL built-in methods.

## Implementation  
Execution begins by constructing an initial VM state, allocating an instance of
the program’s entry class, invoking its constructor, and pushing its entry
method frame onto the call stack. The interpreter loop fetches the next
instruction, increments the program counter, and performs the operation encoded
by the opcode. Attribute access loads or stores object fields, locals are read
and written through the frame, and arithmetic relies on unboxed primitive
payloads before re-boxing results into COOL objects. Control-flow operations
adjust the program counter directly through patched offsets. Dispatch operations
extract method identifiers from class dispatch tables, verify receiver
validity, and create new stack frames with the correct argument bindings.
Built-in routines are invoked through a fast path that intercepts calls before
general bytecode execution. The loop continues until the last frame returns and
no further frames remain, at which point the final value is produced as the
result of program execution.

## References  
[1] “The Cool Reference Manual,” Alex Aiken (et al.), Stanford University,
    The COOL Language Project, Jan. 2011.  
[2] A. V. Aho, M. S. Lam, R. S. Sethi, and J. D. Ullman, *Compilers:
    Principles, Techniques, and Tools*, 2nd ed., ch. 7 (“Run-Time
    Environments”), Pearson/Addison-Wesley, 2006.  