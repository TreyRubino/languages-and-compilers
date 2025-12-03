(*
@author Trey Rubino
@date 12/02/2025
*)

open Runtime
open Error

let maybe_handle_builtin (st : vm_state) (frame : frame) : value option =
  let m = frame.method_info in 
  match m.name with 
  | "abort" -> 
    Printf.printf "abort\n"; exit 1
  | "type_name" -> 
    let cid = frame.self_obj.class_id in 
    let cname = st.ir.classes.(cid).name in
    Some (VString cname)
  | "copy" -> 
    let obj = frame.self_obj in
    let fields = Array.copy obj.fields in
    let new_obj = {
      class_id = obj.class_id;
      fields
    } in
    st.heap <- new_obj :: st.heap;
    Some (VObj new_obj)
  | "out_init" -> 
    let v = frame.locals.(0) in
    (match v with
    | VInt i -> print_int i; print_newline ()
    | _ -> Error.vm "0" "out_init expected Int");
    Some (VObj frame.self_obj)
  | "out_string" -> 
    let v = frame.locals.(0) in
    (match v with
    | VString s -> print_string s; print_newline ()
    | _ -> Error.vm "0" "out_string expected String");
    Some (VObj frame.self_obj)
  | "in_init" -> 
    let line = read_line () in
    let i = int_of_string line in
    Some (VInt i)
  | "in_string" -> 
    let s = read_line () in
    Some (VString s)
  | "concat" -> 
    let arg = 
      match frame.locals.(0) with
      | VString s -> s
      | _ -> Error.vm "0" "concat expected String argument" 
    in
    (match frame.self_obj.fields.(0) with
    | VString base -> 
      Some (VString (base ^ arg))
    | _ -> Error.vm "0" "concat on non-string")
  | _ -> None