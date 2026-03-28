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
  method_ids : ((string * string), int) Hashtbl.t;
  init_ids  : (string, int) Hashtbl.t;
}

let create () = {
  consts  = ref [];
  classes = ref [];
  methods = ref [];
  class_ids = Hashtbl.create 255;
  method_ids = Hashtbl.create 255;
  init_ids = Hashtbl.create 255;
}

let add_const st lit = 
  let id = List.length !(st.consts) in
  st.consts := !(st.consts) @ [lit];
  id 

let add_method st m =
  let id = List.length !(st.methods) in
  st.methods := !(st.methods) @ [m];
  id

let set_method st id m =
  let rec replace i = function
    | [] -> failwith "set_method: bad method id"
    | x :: xs ->
      if i = id then m :: xs
      else x :: replace (i + 1) xs
  in
  st.methods := replace 0 !(st.methods)

let add_class st c =
  st.classes := !(st.classes) @ [c]

let get_method st id =
  try List.nth !(st.methods) id
  with _ -> failwith ("get_method: bad id " ^ string_of_int id)

let get_class st id =
  try List.nth !(st.classes) id
  with _ -> failwith ("get_class: bad id " ^ string_of_int id)

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


