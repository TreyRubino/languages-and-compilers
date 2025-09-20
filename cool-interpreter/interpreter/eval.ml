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
  | _ -> runtime_error loc "arithmetic/comparison on non-Int"

let bool_of_value (loc : string) (v : value) : bool =
  match v with
  | VBool b -> b
  | _ -> runtime_error loc "boolean operation on non-Bool"

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
  | Lt (lhs, rhs) ->
    let lhs_v = eval env ~self ~scopes lhs in
    let rhs_v = eval env ~self ~scopes rhs in
    let lhs_i = int_of_value e.loc lhs_v in
    let rhs_i = int_of_value e.loc rhs_v in
    VBool (lhs_i < rhs_i)
  | Le (lhs, rhs) ->
    let lhs_v = eval env ~self ~scopes lhs in
    let rhs_v = eval env ~self ~scopes rhs in
    let lhs_i = int_of_value e.loc lhs_v in
    let rhs_i = int_of_value e.loc rhs_v in
    VBool (lhs_i <= rhs_i)
  | Equals (lhs, rhs) ->
    let lhs_v = eval env ~self ~scopes lhs in
    let rhs_v = eval env ~self ~scopes rhs in
    (match (lhs_v, rhs_v) with
    | VInt i1, VInt i2 -> VBool (i1 = i2)
    | VBool b1, VBool b2 -> VBool (b1 = b2)
    | VString s1, VString s2 -> VBool (s1 = s2) (* structural eqaulity *)
    | VObj o1, VObj o2 -> VBool (o1 == o2) (* obj physical equality *)
    | VVoid, _ | _, VVoid -> VBool false
    | _ -> VBool (lhs_v == rhs_v)) (* fallback to reference equality *)
  | Isvoid expr ->
    let expr_v = eval env ~self ~scopes expr in
    (match expr_v with
    | VVoid -> VBool true
    | _ -> VBool false)
  | Tilde expr ->
    let expr_v = eval env ~self ~scopes expr in
    let expr_i = int_of_value e.loc expr_v in 
    VInt (-expr_i)
  | Not expr -> 
    let expr_v = eval env ~self ~scopes expr in
    let expr_b = bool_of_value e.loc expr_v in
    VBool (not expr_b)
  | Block exprs -> 
    let rec eval_list (exprs : expr list) : value = 
      match exprs with
      | [] -> VVoid
      | [last] -> eval env ~self ~scopes last
      | head :: tail -> 
        ignore (eval env ~self ~scopes head); 
        eval_list tail
    in 
    eval_list exprs
  | DynamicDispatch _
  | StaticDispatch _
  | SelfDispatch _
  | Let _ 
  | Case _ 
  | If _ 
  | While _ 
  | New _ -> 
      runtime_error e.loc "unimplemented"