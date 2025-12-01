(*
@author Trey Rubino
@date 11/30/2025
*)

type value = 
  | VInt of int
  | VBool of bool
  | VString of string
  | VObj of obj
  | VVoid

and obj = {
  class_id : int; 
  fields : value array
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

let new_object (cls : Ir.class_info) : obj = 
  let nfields = Array.length cls.attributes in
  {
    class_id = cls.id;
    fields = Array.make nfields VVoid;
  }

let create_vm (ir : Ir.ir) : vm_state = 
  {
    ir;
    stack = [];
    frames = [];
    heap = [];
  }