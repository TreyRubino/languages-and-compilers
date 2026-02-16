(*
@author Trey Rubino
@date 11/15/2025
*)

open Semantics
open Gen
open Lower
open Bytecode
open Debug

(* Helper for sorting *)
let rec depth env cname = 
  if cname = "Object" then 0 
  else 
    let parent = 
      try Hashtbl.find env.parent_map cname
      with Not_found -> "Object"
    in
    1 + depth env parent

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
    let mid = Gen.add_method st {
      Ir.name = "__init_" ^ cname;
      class_id;
      n_locals = 0;
      n_formals = 0;
      code = [||];   (* empty, will be replaced *)
    } in
    Hashtbl.replace st.init_ids cname mid
  ) class_names;

  List.iter (fun cname ->
    Lower.scan_method_ids st env cname
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