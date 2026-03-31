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

(** @brief Scans the IR class table to map a human-readable class name to its 
           internal integer identifier. This is used during allocation to 
           ensure the correct memory layout and dispatch table are used.
    @param st The current global VM state.
    @param name The name of the class to look up.
    @return The unique integer class ID. *)
let find_class_id (st : vm_state) (name : string) : int =
  let rec go i =
    if i >= Array.length st.ir.classes then
      Error.vm "0" "missing class %s" name
    else if st.ir.classes.(i).name = name then i
    else go (i + 1)
  in
  go 0

(** @brief Reserves a block of memory on the raw slab for a new COOL object. 
           It checks the GC threshold before allocation, determines the 
           required field count from the class metadata, and handles the 
           initialization of the special String class index.
    @param st The current global VM state.
    @param class_id The identifier for the class being instantiated.
    @return The slab word offset of the freshly allocated object. *)
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

(** @brief Creates a new COOL String object on the slab and populates its 
           primary field with an index from the parallel string table. 
           It performs content interning to ensure string deduplication.
    @param st The current global VM state.
    @param s The OCaml string content to be stored.
    @return The slab word offset of the new String object. *)
let allocate_string (st : vm_state) (s : string) : int =
  if Heap.needs_gc st.heap then Gc.collect st;
  let str_cid = find_class_id st "String" in
  let p   = Heap.alloc st.heap str_cid 1 in
  let idx = Strings.intern st.strings s in
  Heap.set_str_field st.heap p idx;
  p

(** @brief A high-level routine that allocates a COOL object and immediately 
           queues its constructor (__init) on the call stack. This ensures 
           that the object's attributes are initialized according to the 
           class definition before it is used.
    @param st The current global VM state.
    @param class_id The identifier for the class to instantiate and initialize.
    @return The slab word offset of the object awaiting initialization. *)
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