(*
@author Trey Rubino
@date 11/15/2025
*)

open Semantics
open Gen
open Lower
open Bytecode
open Debug

let emit (env : Semantics.semantic_env) : Ir.ir =
  let st = Gen.create () in

  let class_names =
    Hashtbl.fold (fun cname _ acc -> cname :: acc) env.class_map [] 
    |> List.sort (fun c1 c2 ->
        compare (depth env c1) (depth env c2)
      )
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

  let entry_id =
    let found = ref None in
    List.iteri (fun i (m : Ir.method_info) ->
      if m.name = "main" && not (String.starts_with ~prefix:"__init_" m.name)
        then found := Some i
    ) !(st.methods);
    match !found with
    | Some id -> id
    | None -> Error.codegen "0" "Main.main not found"
  in

  Gen.to_ir st entry_id 

