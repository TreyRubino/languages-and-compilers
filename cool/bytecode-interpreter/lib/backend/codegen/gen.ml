(**
@file   gen.ml
@brief  Tools for generating readable dumps of class layouts, dispatch
        tables, constants, and bytecode.
@author Trey Rubino
@date   11/30/2025
*)

open Ir 

type t = {
  consts : literal list ref;
  classes : class_info list ref;
  methods : method_info list ref;
  class_ids : (string, int) Hashtbl.t;
  method_ids : ((string * string), int) Hashtbl.t;
  init_ids  : (string, int) Hashtbl.t;
}

(** @brief Initializes the global generation state. This includes mutable references 
           for the IR components and hash tables to map class and method names 
           to their final integer identifiers.
    @return A fresh generation state [t]. *)
let create () = {
  consts  = ref [];
  classes = ref [];
  methods = ref [];
  class_ids = Hashtbl.create 255;
  method_ids = Hashtbl.create 255;
  init_ids = Hashtbl.create 255;
}

(** @brief Adds a literal (Int, Bool, or String) to the global constant pool. 
           Returns the index of the constant, which bytecode instructions 
           will use to reference it.
    @param st The global generation state.
    @param lit The IR literal to store.
    @return The unique integer index in the constant pool. *)
let add_const st lit = 
  let id = List.length !(st.consts) in
  st.consts := !(st.consts) @ [lit];
  id 

(** @brief Registers a new method in the global method table and returns its 
           unique method identifier.
    @param st The global generation state.
    @param m The method metadata and bytecode.
    @return The unique integer method ID. *)
let add_method st m =
  let id = List.length !(st.methods) in
  st.methods := !(st.methods) @ [m];
  id

(** @brief Updates an existing method's information in the global table. This 
           is commonly used to replace a method stub with its fully lowered 
           bytecode once processing is complete.
    @param st The global generation state.
    @param id The ID of the method to update.
    @param m The new method information. *)
let set_method st id m =
  let rec replace i = function
    | [] -> failwith "set_method: bad method id"
    | x :: xs ->
      if i = id then m :: xs
      else x :: replace (i + 1) xs
  in
  st.methods := replace 0 !(st.methods)

(** @brief Appends a completed class definition, including its attribute 
           layout and dispatch table, to the global class list.
    @param st The global generation state.
    @param c The class information to store. *)
let add_class st c =
  st.classes := !(st.classes) @ [c]

(** @brief Retrieves the metadata for a specific method using its identifier.
    @param st The global generation state.
    @param id The unique method ID.
    @return The method_info record. *)
let get_method st id =
  try List.nth !(st.methods) id
  with _ -> failwith ("get_method: bad id " ^ string_of_int id)

(** @brief Retrieves the metadata for a specific class using its identifier.
    @param st The global generation state.
    @param id The unique class ID.
    @return The class_info record. *)
let get_class st id =
  try List.nth !(st.classes) id
  with _ -> failwith ("get_class: bad id " ^ string_of_int id)

(** @brief Consolidates all accumulated lists (constants, classes, and methods) 
           into fixed-size arrays to produce the final, immutable Intermediate 
           Representation for the VM.
    @param st The global generation state.
    @param entry_id The ID of the Main.main method where execution begins.
    @return The finalized Ir.ir structure. *)
let to_ir st entry_id =
  let consts  = Array.of_list !(st.consts) in
  let classes = Array.of_list !(st.classes) in
  let methods = Array.of_list !(st.methods) in
  {
    Ir.consts;
    classes;
    methods;
    entry_method = entry_id;
  }


