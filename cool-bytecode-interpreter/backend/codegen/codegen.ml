(*
@author Trey Rubino
@date 11/15/2025
*)

open Semantics
open Gen
open Lower
open Bytecode

let emit (env : Semantics.semantic_env) : Ir.ir =
  let st = Gen.create () in

  let class_names =
    Hashtbl.fold (fun c _ acc -> c :: acc) env.class_map []
    |> List.sort compare
  in

  List.iteri (fun i cname ->
    Hashtbl.replace st.class_ids cname i
  ) class_names;

  List.iter (fun cname ->
    let class_id = Hashtbl.find st.class_ids cname in
    let dummy = {
      Ir.name = "__init_" ^ cname;
      class_id;
      n_locals = 0;
      n_formals = 0;
      code = [| { op = OP_RETURN; arg = NoArg } |];
    } in
    let mid = Gen.add_method st dummy in
    Hashtbl.replace st.init_ids cname mid
  ) class_names;

  List.iter (fun cname ->
    Lower.lower_class_group st env cname
  ) class_names;

  Gen.to_ir st
