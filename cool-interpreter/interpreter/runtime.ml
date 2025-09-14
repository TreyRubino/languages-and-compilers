(*
@author Trey Rubino 
@date 09/14/2025
*)

open Reader

(* 
when the interpreter evalutes an expression, it will produce one of 
the following runtime values
*)
type obj = {
  cls : string;
  fields : (string, value ref) Hashtbl.t
}
and value = 
  | VVoid
  | VInt of int
  | VBool of bool
  | VString of string
  | VObj of obj

(* 
when a value is declared and not explicitly initialized, 
we need to give it a default
*)
let default_of_type (t : string) : value = 
  match t with
  | "Int" -> VInt 0 
  | "Bool" -> VBool false
  | "String" -> VString ""
  | _ -> VVoid (* Object, IO, user classes *)

(*
given a class, walk the parent map up to object
*)
let ancestry (parent_map : (string, string) Hashtbl.t) (cls : string) : string list = 
  let rec go acc c = 
    if c = "Object" then List.rev ("Object" :: acc)
    else
      let p = try Hashtbl.find parent_map c with Not_found -> "Object" in
      go (c :: acc) p
    in 
    go [] cls

(*
get attributes in linearized order (Object->...->cls)
*)
let attributes_linearized (env : runtime_env) (cls : string) : attr_info list =
  let get_attrs c = 
    try Hashtbl.find env.class_map c 
    with Not_found -> []
  in
  ancestry env.parent_map cls
  |> List.map get_attrs
  |> List.concat

(*
run default initializers in ancestor -> descendant order
*)
let new_object_defaults (env : runtime_env) (cls : string) : obj = 
  let fields = Hashtbl.create 31 in
  attributes_linearized env cls
  |> List.iter (fun { aname; atype; _} -> 
      Hashtbl.replace fields aname (ref (default_of_type atype)));
  { cls; fields }

