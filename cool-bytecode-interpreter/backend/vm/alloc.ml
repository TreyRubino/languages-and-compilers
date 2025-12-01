(*
@author Trey Rubino
@date 11/30/2025
*)

open Runtime
open Error
open Ir

let allocate_object (st : vm_state) (class_id : int) : obj =
  let cls = st.ir.classes.(class_id) in
  let nfields = Array.length cls.attributes in
  let default (attr : attr_info) : value = 
    match attr.name with 
    | _ -> 
      match cls.name with 
      | "Int" -> VInt 0 
      | "Bool" -> VBool false
      | "String" -> VString ""
      | _ -> VVoid
  in

  let fields = 
    Array.map (fun attr -> default attr ) cls.attributes
  in 
  let obj = { class_id; fields } in
  st.heap <- obj :: st.heap;
  obj

let allocate_and_init (st : vm_state) (class_id : int) : value =
  let obj = allocate_object st class_id in
  let cls = st.ir.classes.(class_id) in 
  let init_name = "__init_" ^ cls.name in

  let rec find_init i = 
    if i >= Array.length st.ir.methods then
      failwith ("missing constructor for " ^ cls.name)
    else if st.ir.methods.(i).name = init_name then i
    else find_init (i+1)
  in
  let init_mid = find_init 0 in
  Exec.invoke st init_mid [];
  VObj obj