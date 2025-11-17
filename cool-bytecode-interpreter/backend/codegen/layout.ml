(*
@author Trey Rubino
@date 11/16/2025
*)

open Semantics

let ancestry (env : Semantics.semantic_env) (cls : string) : string list =
  let rec go acc c = 
    if c = "Object" then List.rev ("Object" :: acc)
    else 
      let p = 
        try Hashtbl.find env.parent_map c 
        with Not_found -> "Object"
      in
      go (c :: acc) p
  in
  go [] cls
  
let linear_attrs (env : Semantics.semantic_env) (cls : string) : Semantics.attr_impl list =
  ancestry env cls
  |> List.map (fun c ->
    try Hashtbl.find env.class_map c with Not_found -> [])
  |> List.concat

let linear_methods (env : Semantics.semantic_env) (cls : string) : (string * Semantics.method_impl) list =
  ancestry env cls 
  |> List.map (fun c -> 
    match Hashtbl.find_opt env.impl_map c with
    | None -> []
    | Some tbl ->
      Hashtbl.fold (fun m impl acc -> (m, impl) :: acc) tbl [])
  |> List.concat
