(*
@author Trey Rubino
@date 11/30/2025
*)

open Runtime
open Exec
open Ir

let execute (ir : Ir.ir) = 
  let st = Runtime.create_state ir in
  let entry_mid = ir.entry_method in
  Exec.invoke st entry_mid [];
