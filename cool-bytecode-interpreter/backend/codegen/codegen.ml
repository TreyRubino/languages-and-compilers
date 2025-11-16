(*
@author Trey Rubino
@date 11/15/2025
*)

open Semantics
open Gen
open Lower

let emit (env : Semantics.semantic_env) : Ir.ir =
  Lower.lower env
