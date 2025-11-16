(*
@author Trey Rubino
@date 11/16/2025
*)

open Ir 

type t = {
  consts : literal list ref;
  classes : class_info list ref;
  methods : method_info list ref;
  class_ids : (string, int) Hashtbl.t;
}

let create () = {
  consts  = ref [];
  classes = ref [];
  methods = ref [];
  class_ids = Hashtbl.create 255;
}

let add_const st lit = 
  let id = List.length !(st.consts) in
  st.consts := !(st.consts) @ [lit];
  id 

let add_method st m =
  st.methods := !(st.methods) @ [m]

let add_class st c =
  st.classes := !(st.classes) @ [c]

let to_ir st =
  let consts  = Array.of_list !(st.consts) in
  let classes = Array.of_list !(st.classes) in
  let methods = Array.of_list !(st.methods) in
  {
    consts;
    classes;
    methods;
    entry_method = 0;  (* temporary *)
  }


