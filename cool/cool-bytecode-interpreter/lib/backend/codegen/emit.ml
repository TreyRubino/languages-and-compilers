(*
@author Trey Rubino
@date 11/16/2025
*)

(* Emit.ml *)
open Bytecode

type t = {
  mutable code : instruction list;
  mutable line_map : (int * int) list; (* (PC, Line) *)
  mutable last_line : int;
}

let create () = { 
  code = []; 
  line_map = []; 
  last_line = -1; 
} 

let record_line buf loc_str =
  let line = try int_of_string loc_str with _ -> 0 in
  let current_pc = List.length buf.code in
  if line <> buf.last_line && line <> 0 then begin
    buf.line_map <- (current_pc, line) :: buf.line_map;
    buf.last_line <- line
  end

let emit_op buf op loc =
  record_line buf loc;
  buf.code <- buf.code @ [{ op; arg = NoArg }]

let emit_op_i buf op n loc = 
  record_line buf loc;
  buf.code <- buf.code @ [{ op; arg = IntArg n }]

let mark buf = List.length buf.code 

let patch buf idx op arg = 
  let rec upd i = function
    | [] -> []
    | x :: xs -> 
      if i = idx then { op; arg } :: xs
      else x :: upd (i + 1) xs
  in
  buf.code <- upd 0 buf.code
  
let to_program buf = 
  Array.of_list buf.code

let get_line_map buf =
  Array.of_list (List.rev buf.line_map)