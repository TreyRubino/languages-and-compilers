(*
@author Trey Rubino
@date 11/16/2025
*)

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

let dispatch_slot env cname mname =
  let meths = linear_methods env cname in
  let rec find i = function
    | [] -> Error.codegen "" "no method %s found in %s" mname cname
    | (n, _) :: xs -> if n = mname then i else find (i + 1) xs
  in
  find 0 meths

let rec linear_tags (ctx : lower_ctx)  (cname : string) =
  let tag = Hashtbl.find ctx.st.class_ids cname in
  let parent = 
    try Hashtbl.find ctx.env.parent_map cname
    with Not_found -> "Object"
  in
  if parent = cname then [tag]
  else tag :: linear_tags ctx parent

let rec depth env cname = 
  if cname = "Object" then 0 
  else 
    let parent = 
      try Hashtbl.find env.parent_map cname
      with Not_found -> "Object"
    in
    1 + depth env parent

let lower_attr a offset =
  { name = a.aname; offset }

let lower_class st env cname attrs _methods =
  let id = Hashtbl.find st.class_ids cname in

  let parent =
    match Hashtbl.find_opt env.parent_map cname with
    | Some p -> p
    | None -> "Object"
  in

  let parent_id =
    match Hashtbl.find_opt st.class_ids parent with
    | Some pid -> pid
    | None -> -1
  in

  (* attributes *)
  let all_attrs = linear_attrs env cname in
  let attrs_arr =
    Array.mapi (fun i a -> { name = a.aname; offset = i })
      (Array.of_list all_attrs)
  in

  (* parent dispatch *)
  let parent_disp =
    if parent <> cname && parent_id >= 0 then
      (Gen.get_class st parent_id).dispatch |> Array.to_list
    else
      []
  in

  let disp = ref parent_disp in
  let meths = linear_methods env cname in

  List.iter (fun (mname, impl) ->
    match Hashtbl.find_opt st.method_ids (impl.definer, mname) with
    | None -> ()
    | Some mid ->
      let rec update = function
        | [] -> (false, [])
        | smid :: xs ->
          if (Gen.get_method st smid).name = mname then
            (true, mid :: xs)
          else
            let (found, rest) = update xs in
            (found, smid :: rest)
      in
      let (found, new_disp) = update !disp in
      if found then disp := new_disp else disp := !disp @ [mid]
  ) meths;

  {
    name = cname;
    id;
    parent_id;
    attributes = attrs_arr;
    dispatch = Array.of_list !disp;
  }

let rec lower_expr (ctx : lower_ctx) (expr : Ast.expr) =
  match expr.expr_kind with
  | Integer s ->
    let id = Gen.add_const ctx.st (Ir.LInt (int_of_string s)) in
    emit_op_i ctx.buf OP_CONST id

  | String s ->
    let id = Gen.add_const ctx.st (Ir.LString s) in
    emit_op_i ctx.buf OP_CONST id

  | True ->
    emit_op ctx.buf OP_TRUE

  | False ->
    emit_op ctx.buf OP_FALSE

  | Identifier ((vloc, vname)) ->
    if vname = "self" then (
      emit_op ctx.buf OP_GET_SELF
    ) else (
      match Hashtbl.find_opt ctx.frame.slot_env vname with
      | Some slot ->
        emit_op_i ctx.buf OP_GET_LOCAL slot
      | None ->
        let attrs = linear_attrs ctx.env ctx.cname in
        let rec find i = function
          | [] -> Error.codegen vloc "unknown identifier %s" vname
          | a :: tl -> if a.aname = vname then i else find (i + 1) tl
        in
        let offset = find 0 attrs in
        emit_op ctx.buf OP_GET_SELF;
        emit_op_i ctx.buf OP_GET_ATTR (offset + 1)
    )

  | Assign ((aloc, aname), rhs) ->
    if aname = "self" then (
      Error.codegen aloc "cannot assign to self"
    ) else (
      match Hashtbl.find_opt ctx.frame.slot_env aname with
      | Some slot ->
        lower_expr ctx rhs;
        emit_op_i ctx.buf OP_SET_LOCAL slot;
        emit_op_i ctx.buf OP_GET_LOCAL slot
      | None ->
        let attrs = linear_attrs ctx.env ctx.cname in
        let rec find i = function
          | [] -> Error.codegen aloc "unknown identifier %s" aname
          | a :: tl -> if a.aname = aname then i else find (i + 1) tl
        in
        let offset = find 0 attrs in
        emit_op ctx.buf OP_GET_SELF; 
        lower_expr ctx rhs;
        emit_op_i ctx.buf OP_SET_ATTR (offset + 1);
        emit_op ctx.buf OP_GET_SELF; 
        emit_op_i ctx.buf OP_GET_ATTR (offset + 1)
    )

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
    patch ctx.buf jf OP_JUMP_IF_FALSE (OffsetArg (mark ctx.buf - jf - 1));
    lower_expr ctx e;
    patch ctx.buf je OP_JUMP (OffsetArg (mark ctx.buf - je - 1))

  | While (pred, body) ->
    let top = mark ctx.buf in
    lower_expr ctx pred;
    let jf = mark ctx.buf in
    emit_op_i ctx.buf OP_JUMP_IF_FALSE 0;
    lower_expr ctx body;
    emit_op ctx.buf OP_POP;
    emit_op_i ctx.buf OP_JUMP (top - mark ctx.buf - 1);
    patch ctx.buf jf OP_JUMP_IF_FALSE (OffsetArg (mark ctx.buf - jf -1));
    emit_op ctx.buf OP_VOID

  | Let (bindings, body) ->
    let fl_child = {
      slot_env = Hashtbl.copy ctx.frame.slot_env;
      next_slot = ctx.frame.next_slot;
      local_count = ctx.frame.local_count;
    } in
    let ctx_child = { ctx with frame = fl_child } in
    List.iter (fun ((_, vname), _, init_opt) ->
      let slot = Layout.allocate_local fl_child vname in
      match init_opt with
      | None -> ()
      | Some e ->
        lower_expr ctx_child e;
        emit_op_i ctx.buf OP_SET_LOCAL slot
    ) bindings;
    lower_expr ctx_child body

  | Case (scrut, branches) ->
    let s_slot = Layout.allocate_local ctx.frame "_scrut" in
    lower_expr ctx scrut;
    emit_op_i ctx.buf OP_SET_LOCAL s_slot;

    emit_op_i ctx.buf OP_GET_LOCAL s_slot;
    
    emit_op ctx.buf OP_ISVOID;
    
    let j_not_void = mark ctx.buf in
    emit_op_i ctx.buf OP_JUMP_IF_FALSE 0;

    emit_op ctx.buf OP_CASE_ABORT;

    let match_start = mark ctx.buf in
    patch ctx.buf j_not_void OP_JUMP_IF_FALSE (OffsetArg (match_start - j_not_void - 1));

    (* load dynamic class tag into a new local frame slot *)
    let t_slot = Layout.allocate_local ctx.frame "_tag" in
    emit_op_i ctx.buf OP_GET_LOCAL s_slot;
    emit_op_i ctx.buf OP_GET_ATTR 0;
    emit_op_i ctx.buf OP_SET_LOCAL t_slot;

    (* sort the branches deepest first *)
    let sorted = 
      List.sort (fun ((_, _), (_, t1), _) ((_, _), (_, t2), _) -> 
        compare (depth ctx.env t2) (depth ctx.env t1)
      ) branches
    in

    let end_jumps = ref [] in

    (* Iterate branches and chain them *)
    List.iter (fun ((_, vname), (_, tname), br) ->
      let branch_tag = Hashtbl.find ctx.st.class_ids tname in
      let tag_const_id = Gen.add_const ctx.st (Ir.LInt branch_tag) in
            
      emit_op_i ctx.buf OP_GET_LOCAL t_slot;
      emit_op_i ctx.buf OP_CONST tag_const_id;
      emit_op ctx.buf OP_EQUAL;
      
      let j_next = mark ctx.buf in
      emit_op_i ctx.buf OP_JUMP_IF_FALSE 0;
      
      let slot = Layout.allocate_local ctx.frame vname in
      emit_op_i ctx.buf OP_GET_LOCAL s_slot;
      emit_op_i ctx.buf OP_SET_LOCAL slot;
      
      lower_expr ctx br;
      
      let j_end = mark ctx.buf in
      emit_op_i ctx.buf OP_JUMP 0;
      end_jumps := j_end :: !end_jumps;
      
      patch ctx.buf j_next OP_JUMP_IF_FALSE (OffsetArg (mark ctx.buf - j_next - 1));
      
    ) sorted;
    
    emit_op ctx.buf OP_CASE_ABORT;
    
    let end_pos = mark ctx.buf in
    List.iter (fun j -> patch ctx.buf j OP_JUMP (OffsetArg (end_pos - j - 1))) !end_jumps

  | New ((cloc, cname)) ->
    (match cname with
    | "SELF_TYPE" ->
      emit_op ctx.buf OP_NEW_SELF_TYPE          (* alloc + init in VM *)
    | _ ->
      let cid =
        try Hashtbl.find ctx.st.class_ids cname
        with Not_found -> Error.codegen cloc "unknown class %s" cname
      in
      emit_op_i ctx.buf OP_NEW cid;             (* allocate object *)
      let init_mid =
        try Hashtbl.find ctx.st.init_ids cname
        with Not_found -> Error.codegen cloc "missing init %s" cname
      in
      emit_op_i ctx.buf OP_CALL init_mid        
    )

  | SelfDispatch ((_mloc, mname), args) ->
    List.iter (fun a -> lower_expr ctx a) args;  (* push args first *)
    emit_op ctx.buf OP_GET_SELF;                (* receiver on top *)
    let slot = dispatch_slot ctx.env ctx.cname mname in
    emit_op_i ctx.buf OP_DISPATCH slot

| DynamicDispatch (recv, (_mloc, mname), args) ->
    List.iter (fun a -> lower_expr ctx a) args;  (* push args first *)
    lower_expr ctx recv;                         (* receiver on top *)

    let cname =
      match recv.static_type with
      | Some (Class c) -> c
      | Some (SELF_TYPE c) -> c
      | _ -> Error.codegen recv.loc "no static type for dispatch"
    in

    let slot = dispatch_slot ctx.env cname mname in
    emit_op_i ctx.buf OP_DISPATCH slot

  | StaticDispatch (recv, (_cloc, tname), (mloc, mname), args) ->
    List.iter (fun a -> lower_expr ctx a) args;  (* push args first *)
    lower_expr ctx recv;                         (* receiver on top *)

    let meths = linear_methods ctx.env tname in
    let impl =
      let rec find = function
        | [] -> Error.codegen mloc "method %s not found in %s" mname tname
        | (name, impl) :: tl -> if name = mname then impl else find tl
      in
      find meths
    in

    let mid =
      try Hashtbl.find ctx.st.method_ids (impl.definer, mname)
      with Not_found ->
        Error.codegen mloc "missing method id for %s.%s" impl.definer mname
    in
    emit_op_i ctx.buf OP_STATIC_DISPATCH mid

  | Block exprs ->
    let fl_child = {
      slot_env = Hashtbl.copy ctx.frame.slot_env;
      next_slot = ctx.frame.next_slot;
      local_count = ctx.frame.local_count;
    } in
    let ctx_child = { ctx with frame = fl_child } in

    let rec emit_block_body = function 
    | [] -> emit_op ctx.buf OP_VOID (* defensive but this shouldnt happen in a vlaid AST *)
    | [last] -> lower_expr ctx_child last
    | h :: t -> 
      lower_expr ctx_child h;
      emit_op ctx.buf OP_POP;
      emit_block_body t
    in
    emit_block_body exprs

let lower_constructor (st : Gen.t) (env : Semantics.semantic_env) (cname : string) (attrs : Semantics.attr_impl list) 
: Ir.method_info =
  let buf = Emit.create () in
  let frame = Layout.create_frame_layout [] in
  let ctx = { st; buf; env; cname; frame } in

  let parent =
    match Hashtbl.find_opt env.parent_map cname with
    | Some p -> p
    | None -> "Object"
  in
  if parent <> cname then (
    match Hashtbl.find_opt st.init_ids parent with
    | None -> ()
    | Some parent_init_mid ->
      emit_op ctx.buf OP_GET_SELF;
      emit_op_i ctx.buf OP_CALL parent_init_mid;
      emit_op ctx.buf OP_POP
  );

  let all_attrs = linear_attrs env cname in
  let offset_of_attr aname =
    let rec find i = function
      | [] -> Error.codegen "" "unknown attribute %s" aname
      | a :: tl -> if a.aname = aname then i else find (i + 1) tl
    in
    find 0 all_attrs
  in

  List.iter (fun a ->
    match a.init with
    | None -> ()
    | Some e ->
      let off = offset_of_attr a.aname in
      emit_op ctx.buf OP_GET_SELF;
      lower_expr ctx e;
      emit_op_i ctx.buf OP_SET_ATTR (off + 1)
  ) attrs;

  emit_op ctx.buf OP_GET_SELF;
  emit_op ctx.buf OP_RETURN;

  {
    name = "__init_" ^ cname;
    class_id = Hashtbl.find st.class_ids cname;
    n_locals = 0;
    n_formals = 0;
    code = Emit.to_program buf;
  }

let lower_method st env cname mname impl =
  let buf = Emit.create () in
  let frame = Layout.create_frame_layout impl.formals in
  let ctx = { st; buf; env; cname; frame } in

  if cname = "Main" && mname = "main" then (
    let init_mid = Hashtbl.find st.init_ids "Main" in
    emit_op ctx.buf OP_GET_SELF;
    emit_op_i ctx.buf OP_CALL init_mid;
    emit_op ctx.buf OP_POP
  );

  (match impl.body with
  | Internal _ ->
    emit_op buf OP_RETURN
  | User body ->
    lower_expr ctx body;
    emit_op buf OP_RETURN);

  let n_formals =
    match impl.body with
    | Internal { qname; _ } ->
      (match qname with
      | "IO.out_int" -> 1
      | "IO.out_string" -> 1
      | "String.concat" -> 1
      | "String.substr" -> 2
      | _ -> 0)
    | User _ ->
      List.length impl.formals
  in
  {
    name = mname;
    class_id = Hashtbl.find st.class_ids cname;
    n_locals = !(frame.local_count);
    n_formals;
    code = Emit.to_program buf;
  }

let scan_method_ids st env cname =
  let meths = linear_methods env cname in
  List.iter (fun (mname, impl) ->
    if not (String.starts_with ~prefix:"__init_" mname) then (
      if impl.definer = cname then (
        let mid = Gen.add_method st {
          Ir.name = mname; class_id = Hashtbl.find st.class_ids cname;
          n_locals=0; n_formals=0; code=[||]
        } in
        Hashtbl.replace st.method_ids (cname, mname) mid
      )
    )
  ) meths

let lower_class_group st env cname =
  let attrs =
    try Hashtbl.find env.class_map cname with _ -> []
  in

  (* constructor *)
  let construct = lower_constructor st env cname attrs in
  let construct_id = Hashtbl.find st.init_ids cname in
  Gen.set_method st construct_id construct;
  
  let meths = linear_methods env cname in
  List.iter (fun (mname, impl) ->
    if not (String.starts_with ~prefix:"__init_" mname) then (
      if impl.definer = cname then (
        let mi = lower_method st env cname mname impl in
        let mid = Hashtbl.find st.method_ids (cname, mname) in
        Gen.set_method st mid mi
      )
    )
  ) meths;
  let class_info = lower_class st env cname attrs meths in
  (*Printf.printf "--- dispatch for %s ---\n" cname;
  Array.iteri (fun i mid ->
    let m = Gen.get_method st mid in
    Printf.printf "  slot %d -> %d.%s\n" i m.class_id m.name
  ) class_info.dispatch;
  flush stdout;*)

  Gen.add_class st class_info;