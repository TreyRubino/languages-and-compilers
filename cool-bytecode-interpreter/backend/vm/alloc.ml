(*
@author Trey Rubino
@date 11/30/2025
*)

open Runtime
open Stack
open Error
open Ir

let find_class_id (st : vm_state) (name : string) : int =
  let rec go i =
    if i >= Array.length st.ir.classes then
      Error.vm "0" "missing class %s" name
    else if st.ir.classes.(i).name = name then i
    else go (i + 1)
  in
  go 0

let allocate_object (st : vm_state) (class_id : int) : obj =
  let cls = st.ir.classes.(class_id) in
  let n_attrs = Array.length cls.attributes in
  let fields = Array.make (n_attrs + 1) VVoid in

  let payload =
    match cls.name with
    | "Int" -> PInt 0
    | "Bool" -> PBool false
    | "String" -> PString ""
    | _ -> PNormal
  in
  let obj = { class_id; fields; payload; marked = false; } in
  st.heap <- obj :: st.heap;

  let int_class_id = find_class_id st "Int" in
  let tag_obj = {
    class_id = int_class_id;
    fields = [||];
    payload = PInt class_id;
    marked = false;
  } in
  st.heap <- tag_obj :: st.heap;
  fields.(0) <- VObj tag_obj;
  obj

let allocate_and_init (st : vm_state) (class_id : int) : value =
  let obj = allocate_object st class_id in
  let cls = st.ir.classes.(class_id) in
  let init_name = "__init_" ^ cls.name in
  let rec find_init i =
    if i >= Array.length st.ir.methods then
      Error.vm "0" "missing constructor for %s" cls.name
    else if st.ir.methods.(i).name = init_name then i
    else find_init (i+1)
  in
  let init_mid = find_init 0 in
  Stack.push_frame st obj init_mid [];
  VObj obj

let box_bool (st : vm_state) (b : bool) : value =
  let cid = find_class_id st "Bool" in
  let o = allocate_object st cid in
  o.payload <- PBool b;
  VObj o

let box_string (st : vm_state) (s : string) : value =
  let cid = find_class_id st "String" in
  let o = allocate_object st cid in
  o.payload <- PString s;
  VObj o

let box_int (st : vm_state) (i : int) : value =
  let cid = find_class_id st "Int" in
  let o = allocate_object st cid in
  o.payload <- PInt i; 
  VObj o