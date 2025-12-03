(*
@author Trey Rubino
@date 11/30/2025
*)

open Runtime 

let push_frame (st : vm_state) (self_obj : Runtime.obj) (method_id : int) (args : value list) : unit =
  let m = st.ir.methods.(method_id) in
  let locals = Array.make (m.n_formals + m.n_locals) VVoid in
  List.iteri (fun i v -> 
    if i < m.n_formals then locals.(i) <- v
    else Error.vm "0" "too many arguments to method"
  ) args;

  let frame = {
    method_info = m;
    locals; 
    pc = 0;
    self_obj;
  } in 
  st.frames <- frame :: st.frames

let pop_frame (st : vm_state) : frame = 
  match st.frames with 
  | fr :: rest ->
    st.frames <- rest;
    fr
  | [] -> 
    Error.vm "0" "call stack underflow"

let peek_frame (st : vm_state) : frame = 
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

let push_val (st : vm_state) (v : value) : unit =
  st.stack <- v :: st.stack

let pop_val (st : vm_state) : value = 
  match st.stack with
  | v :: rest ->
    st.stack <- rest;
    v 
  | [] -> Error.vm "0" "value stack underflow"
  
let peek_val (st : vm_state) : value = 
  match st.stack with
  | v :: _ -> v
  | [] -> Error.vm "0" "value stack empty"
  