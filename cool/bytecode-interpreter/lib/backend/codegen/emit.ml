(**
@file   emit.ml
@brief  Tools for generating readable dumps of class layouts, dispatch
        tables, constants, and bytecode.
@author Trey Rubino
@date   11/30/2025
*)

open Bytecode

type t = {
  mutable code : instruction list;
  mutable line_map : (int * int) list; (* (PC, Line) *)
  mutable last_line : int;
}

(** @brief Initializes a fresh instruction buffer with an empty code sequence 
           and a blank line-to-PC mapping table.
    @return A new, mutable emit state [t]. *)
let create () = { 
  code = []; 
  line_map = []; 
  last_line = -1; 
} 

(** @brief Correlates the current Program Counter (PC) with a source code line 
           number. This mapping is essential for the VM to provide accurate 
           stack traces and error messages during runtime.
    @param buf The current instruction buffer.
    @param loc_str The string representation of the source location (line number). *)
let record_line buf loc_str =
  let line = try int_of_string loc_str with _ -> 0 in
  let current_pc = List.length buf.code in
  if line <> buf.last_line && line <> 0 then begin
    buf.line_map <- (current_pc, line) :: buf.line_map;
    buf.last_line <- line
  end

(** @brief Appends a single, no-argument bytecode operation to the buffer and 
           records the associated source line for debugging.
    @param buf The instruction buffer.
    @param op The opcode to emit.
    @param loc The source location string. *)
let emit_op buf op loc =
  record_line buf loc;
  buf.code <- buf.code @ [{ op; arg = NoArg }]

(** @brief Appends a bytecode operation that carries an integer argument 
           (such as a local slot or class ID) to the buffer.
    @param buf The instruction buffer.
    @param op The opcode to emit.
    @param n The integer operand.
    @param loc The source location string. *)
let emit_op_i buf op n loc = 
  record_line buf loc;
  buf.code <- buf.code @ [{ op; arg = IntArg n }]

(** @brief Captures the current length of the code buffer. This is used as a 
           label or anchor point for calculating jump offsets.
    @param buf The instruction buffer.
    @return The current Program Counter (PC) index. *)
let mark buf = List.length buf.code 

(** @brief Modifies an existing instruction at a specific index. This is 
           primarily used for "back-patching" jump instructions once the 
           target offset is determined later in the codegen process.
    @param buf The instruction buffer.
    @param idx The PC index of the instruction to modify.
    @param op The new opcode (usually the same).
    @param arg The new operand (usually the calculated jump offset). *)
let patch buf idx op arg = 
  let rec upd i = function
    | [] -> []
    | x :: xs -> 
      if i = idx then { op; arg } :: xs
      else x :: upd (i + 1) xs
  in
  buf.code <- upd 0 buf.code
 
(** @brief Converts the accumulated list of instructions into a fixed-size 
           array for high-performance indexing by the VM.
    @param buf The instruction buffer.
    @return An array of Bytecode.instruction records. *)
let to_program buf = 
  Array.of_list buf.code

(** @brief Flattens the accumulated line-number metadata into a sorted array 
           for efficient lookup during runtime error handling.
    @param buf The instruction buffer.
    @return An array of (PC, Line) tuples. *)
let get_line_map buf =
  Array.of_list (List.rev buf.line_map)