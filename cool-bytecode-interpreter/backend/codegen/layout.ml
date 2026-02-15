(*
@author Trey Rubino
@date 11/16/2025
*)

open Semantics

let ancestry (env : Semantics.semantic_env) (cls : string) : string list =
  let rec go acc c = 
    if c = "Object" then ("Object" :: acc)
    else 
      let p = 
        try Hashtbl.find env.parent_map c 
        with Not_found -> "Object"
      in
      go (c :: acc) p
  in
  go [] cls

let linear_methods (env : Semantics.semantic_env) (cls : string) : (string * Semantics.method_impl) list =
  let path = ancestry env cls in
  let table = Hashtbl.create 32 in
  let order = ref [] in
  
  List.iter (fun c ->
    match Hashtbl.find_opt env.impl_map c with
    | None -> ()
    | Some tbl ->
      let current_level_meths = 
        Hashtbl.fold (fun m impl acc -> (m, impl) :: acc) tbl []
        |> List.sort (fun (a, _) (b, _) -> String.compare a b) 
      in
      
      List.iter (fun (mname, impl) ->
        if not (Hashtbl.mem table mname) then (
          order := mname :: !order;
          Hashtbl.add table mname impl
        ) else (
          Hashtbl.replace table mname impl
        )
      ) (List.rev current_level_meths)
  ) path;
  
  List.rev !order |> List.map (fun mname -> (mname, Hashtbl.find table mname))

let linear_attrs (env : Semantics.semantic_env) (cls : string) : Semantics.attr_impl list =
  let path = ancestry env cls in
  let table = Hashtbl.create 32 in
  let order = ref [] in

  List.iter (fun c ->
    let attrs = try Hashtbl.find env.class_map c with Not_found -> [] in
    List.iter (fun a ->
      if not (Hashtbl.mem table a.aname) then (
        order := a.aname :: !order;
        Hashtbl.add table a.aname a
      )
    ) (List.rev attrs) 
  ) path;

  List.rev !order |> List.map (fun name -> Hashtbl.find table name)

type frame_layout = {
  slot_env    : (string, int) Hashtbl.t;
  next_slot   : int ref;
  local_count : int ref;
}

let create_frame_layout (formals : (Ast.id * Ast.cool_type) list) : frame_layout =
  let env = Hashtbl.create 16 in
  List.iteri (fun i ((_, name), _) ->
    Hashtbl.add env name i
  ) formals;
  { 
    slot_env = env;
    next_slot = ref (List.length formals);
    local_count = ref 0;
  }

let allocate_local (fl : frame_layout) (name : string) : int =
  let slot = !(fl.next_slot) in
  Hashtbl.add fl.slot_env name slot;
  incr fl.next_slot;
  incr fl.local_count;
  slot