(*
@author Trey Rubino
@date 11/15/2025
*)

type literal = 
  | LInt of int
  | LBool of bool
  | LString of string
  | LVoid 

type const_table = literal array

type attr_info = {
  name   : string;
  offset : int; 
}

type class_info = {
  name : string;
  id   : int; 
  parent_id  : int;
  attributes : attr_info array;
  dispatch   : int array;
}

type method_info = {
  name : string;
  class_id  : int;
  n_locals  : int;
  n_formals : int;
  code : Bytecode.program;
}

type ir = {
  consts  : const_table; 
  classes : class_info array;
  methods : method_info array;
  entry_method : int;
}

