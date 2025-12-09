(*
@author Trey Rubino
@date 11/30/2025
*)

open Ir

type payload =
  | PNormal
  | PInt of int
  | PBool of bool
  | PString of string

type value =
  | VObj of obj
  | VVoid

and obj = {
  class_id : int;
  fields : value array;
  mutable payload : payload;
  mutable marked : bool;
}

type frame = {
  mutable pc : int;
  method_info : Ir.method_info;
  locals : value array;
  self_obj : obj;
}

type vm_state = {
  ir : Ir.ir;
  mutable stack : value list;
  mutable frames : frame list;
  mutable heap : obj list;
}

let create_vm (ir : Ir.ir) : vm_state = 
  {
    ir;
    stack = [];
    frames = [];
    heap = [];
  }

let string_of_value v =
  match v with
  | VVoid -> "void"
  | VObj o ->
      match o.payload with
      | PInt i -> Printf.sprintf "Int(%d)" i
      | PBool b -> Printf.sprintf "Bool(%b)" b
      | PString s -> Printf.sprintf "String(%s)" s
      | PNormal -> Printf.sprintf "Obj<%d>" o.class_id
