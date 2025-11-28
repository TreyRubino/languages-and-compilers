open Semantics
open Ir
open Gen
open Emit
open Bytecode
open Layout
open Error

type lower_ctx = {
  st    : Gen.t;
  buf   : Emit.t;
  env   : Semantics.semantic_env;
  cname : string;
  frame : Layout.frame_layout;
}

let lower_attr a offset =
  { name = a.aname; offset }

let lower_class st env cname attrs methods =
  let id = Hashtbl.find st.class_ids cname in
  let parent =
    try Hashtbl.find env.parent_map cname with _ -> cname
  in
  let parent_id =
    try Hashtbl.find st.class_ids parent with _ -> -1
  in
  let all_attrs = linear_attrs env cname in
  let attrs_arr =
    Array.mapi (fun i a -> lower_attr a i) (Array.of_list all_attrs)
  in
  let disp =
    Array.init (List.length methods) (fun i -> i)
  in
  {
    name = cname;
    id;
    parent_id;
    attributes = attrs_arr;
    dispatch = disp;
  }

let rec lower_expr (ctx : lower_ctx) (expr : Ast.expr) =
  match expr.expr_kind with
  | Integer s ->
    let n = int_of_string s in
    emit_op_i ctx.buf OP_CONST n

  | String s ->
    let id = Gen.add_const ctx.st (Ir.LString s) in
    emit_op_i ctx.buf OP_CONST id

  | True ->
    emit_op ctx.buf OP_TRUE

  | False ->
    emit_op ctx.buf OP_FALSE

  | Identifier ((vloc, vname)) ->
    let slot = 
      try Hashtbl.find ctx.frame.slot_env vname
      with Not_found -> Error.codegen vloc "unknown local %s" vname
    in
    emit_op_i ctx.buf OP_GET_LOCAL slot

  | Assign ((aloc, aname), rhs) ->
    let slot = 
      try Hashtbl.find ctx.frame.slot_env aname
      with Not_found -> Error.codegen aloc "unknown local %s" aname
    in
    lower_expr ctx rhs;
    emit_op_i ctx.buf OP_SET_LOCAL slot

  | Plus (l, r) ->
    lower_expr ctx l;
    lower_expr ctx r;
    emit_op ctx.buf OP_ADD

  | Minus (l, r) ->
    lower_expr ctx l;
    lower_expr ctx r;
    emit_op ctx.buf OP_SUB

  | Times (l, r) ->
    lower_expr ctx l;
    lower_expr ctx r;
    emit_op ctx.buf OP_MUL

  | Divide (l, r) ->
    lower_expr ctx l;
    lower_expr ctx r;
    emit_op ctx.buf OP_DIV

  | Tilde e ->
    lower_expr ctx e;
    emit_op ctx.buf OP_NEG

  | Lt (l, r) ->
    lower_expr ctx l;
    lower_expr ctx r;
    emit_op ctx.buf OP_LESS

  | Le (l, r) ->
    lower_expr ctx l;
    lower_expr ctx r;
    emit_op ctx.buf OP_LESS_EQUAL

  | Equals (l, r) ->
    lower_expr ctx l;
    lower_expr ctx r;
    emit_op ctx.buf OP_EQUAL

  | Not e ->
    lower_expr ctx e;
    emit_op ctx.buf OP_NOT

  | Isvoid e ->
    lower_expr ctx e;
    emit_op ctx.buf OP_ISVOID

  | If (pred, t, e) ->
    lower_expr ctx pred;
    let jf = mark ctx.buf in
    emit_op_i ctx.buf OP_JUMP_IF_FALSE 0;
    lower_expr ctx t;
    let je = mark ctx.buf in
    emit_op_i ctx.buf OP_JUMP 0;
    patch ctx.buf jf OP_JUMP_IF_FALSE (OffestArg (mark ctx.buf - jf));
    lower_expr ctx e;
    patch ctx.buf je OP_JUMP (OffestArg (mark ctx.buf - je))

  | While (pred, body) ->
    let top = mark ctx.buf in
    lower_expr ctx pred;
    let jf = mark ctx.buf in
    emit_op_i ctx.buf OP_JUMP_IF_FALSE 0;
    lower_expr ctx body;
    emit_op_i ctx.buf OP_LOOP (top - mark ctx.buf);
    patch ctx.buf jf OP_JUMP_IF_FALSE (OffestArg (mark ctx.buf - jf))

  | Let (bindings, body) ->
    let fl_child = {
      slot_env = Hashtbl.copy ctx.frame.slot_env;
      next_slot = ctx.frame.next_slot;
      local_count = ctx.frame.local_count;
    } in 
    let ctx_child = {
      ctx with frame = fl_child
    } in 
    List.iter (fun ((vloc, vname), _, init_opt) ->
      let slot = Layout.allocate_local fl_child vname in
      match init_opt with
      | None -> ()
      | Some e -> 
        lower_expr ctx_child e;
        emit_op_i ctx.buf OP_SET_LOCAL slot
    ) bindings;
    lower_expr ctx_child body

  | Case _ ->
    failwith "TODO: implement Case lowering"

  | New ((cloc, cname)) ->
    let cid = 
      try Hashtbl.find ctx.st.class_ids cname
      with Not_found -> Error.codegen cloc "unknown class %s" cname
    in
    emit_op_i ctx.buf OP_NEW cid

  | SelfDispatch ((_, _), args) ->
    emit_op ctx.buf OP_GET_SELF;
    List.iter (fun a -> lower_expr ctx a) args;
    emit_op_i ctx.buf OP_DISPATCH 0

  | DynamicDispatch (recv, (mloc, mname), args) ->
    lower_expr ctx recv;
    List.iter (fun a -> lower_expr ctx a) args;
    let cname =
      match recv.static_type with
      | Some (Class c) -> c
      | Some (SELF_TYPE c) -> c
      | _ -> Error.codegen recv.loc "receiver has no static type"
    in
    let meths = linear_methods ctx.env cname in
    let slot =
      let rec find i = function
      | [] -> Error.codegen mloc "method %s not found" mname
      | (name, _) :: tl ->
        if name = mname then i else find (i+1) tl
      in
      find 0 meths
    in
    emit_op_i ctx.buf OP_DISPATCH slot

  | StaticDispatch (recv, (_, _), (_, _), args) ->
    lower_expr ctx recv;
    List.iter (fun a -> lower_expr ctx a) args;
    emit_op_i ctx.buf OP_STATIC_DISPATCH 0

  | Block exprs ->
    let fl_child = {
      slot_env = Hashtbl.copy ctx.frame.slot_env;
      next_slot = ctx.frame.next_slot;
      local_count = ctx.frame.local_count;
    } in
    let ctx_child = {
      ctx with frame = fl_child
    } in
    List.iter (fun e -> lower_expr ctx_child e) exprs

let lower_method st env cname mname impl =
  let buf = Emit.create () in
  let n_formals = List.length impl.formals in
  let n_locals = 0 in
  let frame = Layout.create_frame_layout impl.formals in
  let ctx = {
    st = st;
    buf = buf;
    env = env;
    cname = cname;
    frame = frame;
  } in

  (match impl.body with
  | Internal _ ->
    emit_op buf OP_RETURN
  | User body ->
    lower_expr ctx body;
    emit_op buf OP_RETURN);
  {
    name = mname;
    class_id = Hashtbl.find st.class_ids cname;
    n_locals = !(frame.local_count);
    n_formals = List.length impl.formals;
    code = Emit.to_program buf;
  }

let lower_class_group st env cname =
  let attrs =
    try Hashtbl.find env.class_map cname with _ -> []
  in
  let meths = linear_methods env cname in
  let class_info =
    lower_class st env cname attrs meths
  in
  Gen.add_class st class_info;
  List.iter (fun (mname, impl) ->
    if impl.definer = cname then (
      let mi = lower_method st env cname mname impl in
      Gen.add_method st mi
    )
  ) meths