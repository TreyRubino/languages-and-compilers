(*
@author Trey Rubino
@date 11/30/2025
*)

open Runtime
open Exec
open Alloc
open Stack
open Ir

let execute ir =
  let st = Runtime.create_vm ir in
  let entry_m = ir.methods.(ir.entry_method) in
  let main_class_id = entry_m.class_id in
  let main_val = Alloc.allocate_and_init st main_class_id in
  let main_obj =
    match main_val with
    | VObj o -> o
    | _ -> Error.vm "0" "main object is not an object"
  in
  ignore (Exec.run st);
  Stack.push_frame st main_obj ir.entry_method [];
  Exec.run st


