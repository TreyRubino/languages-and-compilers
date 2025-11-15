(*
@author Trey Rubino
@date 11/15/2025
*)

open Ast

type attr_info = {
  aname : string;
  atype : string;
  init  : expr option;
}

type method_body =
  | Internal of { rtype : string; qname : string }
  | User of expr

type method_impl = {
  definer : string;
  formals : string list;
  body    : method_body;
}

type class_attrs   = attr_info list
type class_methods = (string, method_impl) Hashtbl.t

type semantic_env = {
  class_map  : (string, class_attrs) Hashtbl.t;
  impl_map   : (string, class_methods) Hashtbl.t;
  parent_map : (string, string) Hashtbl.t;
}

let empty_env () : semantic_env =
  {
    class_map  = Hashtbl.create 255;
    impl_map   = Hashtbl.create 255;
    parent_map = Hashtbl.create 255;
  }
