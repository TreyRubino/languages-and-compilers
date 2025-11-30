(*
@author Trey Rubino
@date 11/16/2025
*)

open Bytecode

type t = {
  mutable code : instruction list;
}

let create () = { code = [] } 

let emit_op buf op =
  buf.code <- buf.code @ [{ op; arg = NoArg }]

let emit_op_i buf op n = 
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