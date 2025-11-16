(*
@author Trey Rubino
@date 11/15/2025
*)

open Semantics
open Gen
open Lower

let emit (env : Semantics.semantic_env) : Ir.ir =
  let st = Gen.create () in
  
  Hashtbl.iter (fun cname attrs -> 
    let methods = 
      let tbl = Hashtbl.find env.impl_map cname in
      Hashtbl.fold (fun mname impl acc -> (mname, impl) :: acc) tbl []
    in 
    let cinfo = Lower.lower_class st env cname attrs methods in
    Gen.add_class st cinfo;
    List.iter (fun (mname, impl) -> 
      let minfo = Lower.lower_method st env cname mname impl in
      Gen.add_method st minfo
    ) methods
  ) env.class_map;

  {
    Ir.consts = Array.of_list !(st.consts);
    classes   = Array.of_list !(st.classes);
    methods   = Array.of_list !(st.methods);
    entry_method = 0;
  }
