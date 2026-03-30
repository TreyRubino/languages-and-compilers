(**
@file   alloc.ml
@brief  Provides allocation routines for COOL objects. All allocations
        flow through this module into the raw word slab. A collection is
        triggered proactively when the slab's bump pointer reaches the GC
        threshold. Integer and boolean results are unboxed values and never
        allocated here. String objects are created via allocate_string.
@author Trey Rubino
@date   11/30/2025
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

(* allocate a COOL object of the given class on the raw slab.
   triggers a collection if the bump pointer has reached the threshold.
   String class is special: it always gets one slab field for its content
   index, initialized to the empty string. returns the word offset. *)
let allocate_object (st : vm_state) (class_id : int) : int =
  if Heap.needs_gc st.heap then Gc.collect st;
  let cls = st.ir.classes.(class_id) in
  let n_fields =
    if cls.name = "String" then 1    (* one field holds the string-table index *)
    else Array.length cls.attributes
  in
  let p = Heap.alloc st.heap class_id n_fields in
  if cls.name = "String" then begin
    let empty = Strings.intern st.strings "" in
    Heap.set_str_field st.heap p empty
  end;
  p

(* allocate a String object containing the given string content.
   interns the string content in the string table and writes the resulting
   index into field[0] of the new slab object. returns the word offset. *)
let allocate_string (st : vm_state) (s : string) : int =
  if Heap.needs_gc st.heap then Gc.collect st;
  let str_cid = find_class_id st "String" in
  let p   = Heap.alloc st.heap str_cid 1 in
  let idx = Strings.intern st.strings s in
  Heap.set_str_field st.heap p idx;
  p

(* allocate an object and push its constructor frame onto the call stack.
   used only at VM startup for the Main object. returns the word offset. *)
let allocate_and_init (st : vm_state) (class_id : int) : int =
  let p   = allocate_object st class_id in
  let cls = st.ir.classes.(class_id) in
  let init_name = "__init_" ^ cls.name in
  let rec find_init i =
    if i >= Array.length st.ir.methods then
      Error.vm "0" "missing constructor for %s" cls.name
    else if st.ir.methods.(i).name = init_name then i
    else find_init (i + 1)
  in
  let init_mid = find_init 0 in
  Stack.push_frame st p init_mid [];
  p