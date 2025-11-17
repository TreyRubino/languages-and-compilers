open Semantics
open Ir
open Gen
open Emit
open Bytecode
open Layout

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

let rec lower_expr st buf env cname (expr : Ast.expr) =
  match expr.expr_kind with
  | Integer s ->
    let n = int_of_string s in
    emit_op_i buf OP_CONST n
  | String s ->
    let id = Gen.add_const st (Ir.LString s) in
    emit_op_i buf OP_CONST id
  | True ->
    emit_op buf OP_TRUE
  | False ->
    emit_op buf OP_FALSE
  | Identifier (_, _) ->
    emit_op_i buf OP_GET_LOCAL 0
  | Assign ((_, _), rhs) ->
    lower_expr st buf env cname rhs;
    emit_op_i buf OP_SET_LOCAL 0
  | Plus (l, r) ->
    lower_expr st buf env cname l;
    lower_expr st buf env cname r;
    emit_op buf OP_ADD
  | Minus (l, r) ->
    lower_expr st buf env cname l;
    lower_expr st buf env cname r;
    emit_op buf OP_SUB
  | Times (l, r) ->
    lower_expr st buf env cname l;
    lower_expr st buf env cname r;
    emit_op buf OP_MUL
  | Divide (l, r) ->
    lower_expr st buf env cname l;
    lower_expr st buf env cname r;
    emit_op buf OP_DIV
  | Tilde e ->
    lower_expr st buf env cname e;
    emit_op buf OP_NEG
  | Lt (l, r) ->
    lower_expr st buf env cname l;
    lower_expr st buf env cname r;
    emit_op buf OP_LESS
  | Le (l, r) ->
    lower_expr st buf env cname l;
    lower_expr st buf env cname r;
    emit_op buf OP_LESS_EQUAL
  | Equals (l, r) ->
    lower_expr st buf env cname l;
    lower_expr st buf env cname r;
    emit_op buf OP_EQUAL
  | Not e ->
    lower_expr st buf env cname e;
    emit_op buf OP_NOT
  | Isvoid e ->
    lower_expr st buf env cname e;
    emit_op buf OP_ISVOID
  | If (pred, t, e) ->
    lower_expr st buf env cname pred;
    let jf = mark buf in
    emit_op_i buf OP_JUMP_IF_FALSE 0;
    lower_expr st buf env cname t;
    let je = mark buf in
    emit_op_i buf OP_JUMP 0;
    patch buf jf OP_JUMP_IF_FALSE (OffestArg (mark buf - jf));
    lower_expr st buf env cname e;
    patch buf je OP_JUMP (OffestArg (mark buf - je))
  | While (pred, body) ->
    let top = mark buf in
    lower_expr st buf env cname pred;
    let jf = mark buf in
    emit_op_i buf OP_JUMP_IF_FALSE 0;
    lower_expr st buf env cname body;
    emit_op_i buf OP_LOOP (top - mark buf);
    patch buf jf OP_JUMP_IF_FALSE (OffestArg (mark buf - jf))
  | Let _ ->
    failwith "TODO: implement Let lowering"
  | Case _ ->
    failwith "TODO: implement Case lowering"
  | New (_, _) ->
    emit_op_i buf OP_NEW 0
  | SelfDispatch ((_, _), args) ->
    emit_op buf OP_GET_SELF;
    List.iter (fun a -> lower_expr st buf env cname a) args;
    emit_op_i buf OP_DISPATCH 0
  | DynamicDispatch (recv, (_, _), args) ->
    lower_expr st buf env cname recv;
    List.iter (fun a -> lower_expr st buf env cname a) args;
    emit_op_i buf OP_DISPATCH 0
  | StaticDispatch (recv, (_, _), (_, _), args) ->
    lower_expr st buf env cname recv;
    List.iter (fun a -> lower_expr st buf env cname a) args;
    emit_op_i buf OP_STATIC_DISPATCH 0
  | Block exprs ->
    List.iter (fun e -> lower_expr st buf env cname e) exprs

let lower_method st env cname mname impl =
  let buf = Emit.create () in
  let n_formals = List.length impl.formals in
  let n_locals = 0 in
  (match impl.body with
  | Internal _ ->
    emit_op buf OP_RETURN
  | User body ->
    lower_expr st buf env cname body;
    emit_op buf OP_RETURN);
  {
    name = mname;
    class_id = Hashtbl.find st.class_ids cname;
    n_locals;
    n_formals;
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
    let mi = lower_method st env cname mname impl in
    Gen.add_method st mi
  ) meths