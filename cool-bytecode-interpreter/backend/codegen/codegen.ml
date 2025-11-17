(*
@author Trey Rubino
@date 11/15/2025
*)

open Semantics
open Gen
open Lower

let emit (env : Semantics.semantic_env) : Ir.ir =
  let st = Gen.create () in
  let class_names =
    List.sort compare (Hashtbl.fold (fun c _ acc -> c :: acc) env.class_map [])
  in
  List.iteri (fun i cname ->
    Hashtbl.replace st.class_ids cname i
  ) class_names;
  List.iter (fun cname ->
    Lower.lower_class_group st env cname
  ) class_names;
  Gen.to_ir st

