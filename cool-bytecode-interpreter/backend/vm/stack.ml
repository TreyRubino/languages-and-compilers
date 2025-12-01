(*
@author Trey Rubino
@date 11/30/2025
*)

open Runtime 

let push (st : vm_state) (method_id : int) (args : value list) : unit =
  let m = str.ir.methods.(method_id) in
  let locals = Array.make (m.n_formals + m.n_locals) VVoid in
  List.iteri (fun i v -> 
    if i < m.n_formals then local.(i) <- v
    else Error.vm "0" "too many arguments to method"
  ) args;

  let frame = {
    method_id;
    locals; 
    pc = 0;
    self = VVoid;
  } in 
  st.frames <- frame :: st.frames

let pop (st : vm_state) : frame = 
  match st.frames with 
  | fr :: rest ->
    st.frames <- rest;
    fr
  | [] -> 
    Error.vm "0" "call stack underflow"

let peek (st : vm_state) : frame = 
  match st.frames with
  | fr :: _ -> fr
  | [] -> Error.vm "0" "no action frame"

let get_local (st : vm_state) (slot : int) : value = 
  match st.frames with
  | f :: _ -> 
    f.locals.(slot)
  | [] -> 
    Error.vm "0" "no active frame"

let set_local (st : vm_state) (slot : int) (v : value) : unit = 
  match st.frames with
  | f :: _ -> 
    f.locals.(slot) <- v
  | [] -> 
    Error.vm "0" "active frame"

  