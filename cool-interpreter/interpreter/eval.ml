(*
@author Trey Rubino
@date 09/14/2025
*)

open Ast
open Reader
open Runtime

let int_of_value (loc : string) (v : value) : int = 
  match v with
  | VInt i -> i
  | _ -> runtime_error loc "arithmetic on non-Int"

let rec eval (env : runtime_env) ~(self:obj) ~(scopes:scope list) (e : expr) : value = 
  match e.expr_kind with
  | Integer s -> VInt (int_of_string s)
  | String s  -> VString s
  | True      -> VBool true
  | False     -> VBool false
  | Identifier (loc, name) ->
    (match lookup_value ~self scopes name with
    | Some v -> v
    | None -> runtime_error loc ("unbound identifier " ^ name))
  | Assign ((loc, name), rhs) ->
    if name = "self" then
      runtime_error loc "cannot assign to self"
    else 
      let v = eval env ~self ~scopes rhs in
      (match lookup_lvalue_cell ~self scopes name with
      | Some cell -> 
        cell := v; 
        v
      | None -> 
        runtime_error loc ("unbound identifier " ^ name))
  | Plus (lhs, rhs) ->
    let lhs_v = eval env ~self ~scopes lhs in
    let rhs_v = eval env ~self ~scopes rhs in
    let lhs_i = int_of_value e.loc lhs_v in
    let rhs_i = int_of_value e.loc rhs_v in
    VInt (lhs_i + rhs_i)
  | Minus (lhs, rhs) -> 
    let lhs_v = eval env ~self ~scopes lhs in
    let rhs_v = eval env ~self ~scopes rhs in
    let lhs_i = int_of_value e.loc lhs_v in
    let rhs_i = int_of_value e.loc rhs_v in
    VInt (lhs_i - rhs_i)
  | Times (lhs, rhs) -> 
    let lhs_v = eval env ~self ~scopes lhs in
    let rhs_v = eval env ~self ~scopes rhs in
    let lhs_i = int_of_value e.loc lhs_v in
    let rhs_i = int_of_value e.loc rhs_v in
    VInt (lhs_i * rhs_i)
  | Divide (lhs, rhs) -> 
    let lhs_v = eval env ~self ~scopes lhs in
    let rhs_v = eval env ~self ~scopes rhs in
    let lhs_i = int_of_value e.loc lhs_v in
    let rhs_i = int_of_value e.loc rhs_v in
    if rhs_i = 0 then runtime_error e.loc "division by zero"
    else VInt (lhs_i / rhs_i)
  | DynamicDispatch _
  | StaticDispatch _
  | SelfDispatch _
  | Let _ 
  | Case _ 
  | If _ 
  | While _ 
  | New _ 
  | Isvoid _ 
  | Tilde _ 
  | Lt _ 
  | Le _ 
  | Equals _
  | Not _
  | Block _ -> 
      runtime_error e.loc "unimplemented"