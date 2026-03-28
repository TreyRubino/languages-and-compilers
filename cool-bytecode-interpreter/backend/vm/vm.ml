(**
@file   vm.ml
@brief  Entry point for constructing the VM, instantiating the main object,
        and starting program execution.
@author Trey Rubino
@date   11/30/2025
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
  let main_obj = Alloc.allocate_object st main_class_id in
  Stack.push_frame st main_obj ir.entry_method [];
  Exec.run st