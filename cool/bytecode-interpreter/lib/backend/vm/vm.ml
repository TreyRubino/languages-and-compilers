(**
@file   vm.ml
@brief  Entry point for constructing the VM, instantiating the main object,
        invoking its initializer, installing the entry method, and starting
        program execution.
@author Trey Rubino
@date   11/30/2025
*)

open Runtime
open Exec
open Alloc
open Stack
open Ir

let execute (ir : Ir.ir) : value =
  let st            = Runtime.create_vm ir in
  let entry_m       = ir.methods.(ir.entry_method) in
  let main_class_id = entry_m.class_id in
  (* allocate the Main object without pushing an init frame. lower_method
     emits GET_SELF; CALL __init_Main; POP at the top of main(), so the
     constructor is invoked exactly once from there. using allocate_and_init
     here would push a second init frame, causing __init_Main to run twice. *)
  let main_ptr      = Alloc.allocate_object st main_class_id in
  Stack.push_frame st main_ptr ir.entry_method [];
  Exec.run st