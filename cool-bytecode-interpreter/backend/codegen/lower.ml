open Semantics
open Ir
open Gen
open Emit
open Bytecode

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
  let attrs_arr =
    Array.mapi (fun i a -> lower_attr a i) (Array.of_list attrs)
  in

  let disp =
    Array.of_list (List.map (fun (mname, impl) -> 0) methods)
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

  | Isvoid e ->
      lower_expr st buf env cname e;
      emit_op buf OP_ISVOID

  | Identifier (_, name) ->
      (* locals are looked up by offset; for now assume offset=0 *)
      (* later we maintain a local environment *)
      emit_op_i buf OP_GET_LOCAL 0

  | Assign ((_, name), rhs) ->
      lower_expr st buf env cname rhs;
      (* store into local offset 0 for now *)
      emit_op_i buf OP_SET_LOCAL 0

  | Tilde e ->
      lower_expr st buf env cname e;
      emit_op buf OP_NEG

  | Not e ->
      lower_expr st buf env cname e;
      emit_op buf OP_NOT

  | Plus (l, r) ->
      lower_binary st buf env cname l r OP_ADD

  | Minus (l, r) ->
      lower_binary st buf env cname l r OP_SUB

  | Times (l, r) ->
      lower_binary st buf env cname l r OP_MUL

  | Divide (l, r) ->
      lower_binary st buf env cname l r OP_DIV

  | Lt (l, r) ->
      lower_binary st buf env cname l r OP_LESS

  | Le (l, r) ->
      lower_binary st buf env cname l r OP_LESS_EQUAL

  | Equals (l, r) ->
      lower_binary st buf env cname l r OP_EQUAL

  | Block exprs ->
      List.iter (fun e -> lower_expr st buf env cname e) exprs

  | If (pred, then_e, else_e) ->
      lower_expr st buf env cname pred;
      let j_false = mark buf in
      emit_op_i buf OP_JUMP_IF_FALSE 0;
      lower_expr st buf env cname then_e;
      let j_end = mark buf in
      emit_op_i buf OP_JUMP 0;
      patch buf j_false (OP_JUMP_IF_FALSE) (OffestArg (mark buf - j_false));
      lower_expr st buf env cname else_e;
      patch buf j_end OP_JUMP (OffestArg (mark buf - j_end))

  | While (pred, body) ->
      let top = mark buf in
      lower_expr st buf env cname pred;
      let j_false = mark buf in
      emit_op_i buf OP_JUMP_IF_FALSE 0;
      lower_expr st buf env cname body;
      emit_op_i buf OP_LOOP (top - mark buf);
      patch buf j_false OP_JUMP_IF_FALSE (OffestArg (mark buf - j_false))

  | Let _ ->
      failwith "TODO: implement Let lowering"

  | Case _ ->
      failwith "TODO: implement Case lowering"

  | DynamicDispatch (recv, (_, mname), args) ->
      (* receiver *)
      lower_expr st buf env cname recv;

      (* args in order *)
      List.iter (fun a -> lower_expr st buf env cname a) args;

      (* later we add OP_PREPARE_CALL *)
      emit_op_i buf OP_DISPATCH 0

  | StaticDispatch (recv, (_, ty), (_, mname), args) ->
      lower_expr st buf env cname recv;
      List.iter (fun a -> lower_expr st buf env cname a) args;
      emit_op_i buf OP_STATIC_DISPATCH 0

  | SelfDispatch ((_, mname), args) ->
      emit_op buf OP_GET_SELF;
      List.iter (fun a -> lower_expr st buf env cname a) args;
      emit_op_i buf OP_DISPATCH 0

  | New (_, class_name) ->
      emit_op_i buf OP_NEW 0

  | _ ->
      emit_op buf OP_VOID

and lower_binary st buf env cname l r op =
  lower_expr st buf env cname l;
  lower_expr st buf env cname r;
  emit_op buf op

let lower_method st env cname mname impl =
  let buf = Emit.create () in
  let n_formals = List.length impl.formals in
  let n_locals = 0 in
  begin
    match impl.body with
    | Internal _ ->
        emit_op buf OP_RETURN
    | User body ->
        lower_expr st buf env cname body;
        emit_op buf OP_RETURN
  end;
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
  let impls =
    try Hashtbl.find env.impl_map cname with _ -> Hashtbl.create 1
  in
  let meths =
    Hashtbl.fold (fun mname impl acc -> (mname, impl) :: acc) impls []
  in
  let class_info =
    lower_class st env cname attrs meths
  in
  Gen.add_class st class_info;
  List.iter (fun (mname, impl) ->
    let mi = lower_method st env cname mname impl in
    Gen.add_method st mi
  ) meths

let lower env =
  let st = Gen.create () in
  Hashtbl.iter (fun cname _ ->
    Hashtbl.replace st.class_ids cname (List.length !(st.classes))
  ) env.class_map;
  Hashtbl.iter (fun cname _ ->
    lower_class_group st env cname
  ) env.class_map;
  Gen.to_ir st
