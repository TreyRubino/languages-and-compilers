(*
@author Trey Rubino
@date 11/30/2025
*)

open Runtime
open Bytecode
open Builtin
open Stack
open Alloc
open Ir
open Error

let to_int32 (x : int) : int = 
  Int32.to_int (Int32.of_int x)

let expect_int (v : value) : int =
  match v with 
  | VObj o -> 
    (match o.payload with
    | PInt i -> i
    | _ -> Error.vm "1" "expected Int object")
  | VVoid ->
    Error.vm "0" "expected Int object (void)"

let expect_bool (v : value) : bool =
  match v with
  | VObj o ->
    (match o.payload with
    | PBool b -> b
    | _ -> Error.vm "0" "expected Bool object")
  | VVoid ->
    Error.vm "0" "expected Bool object (void)"

let const_of_value (st : vm_state) = function
  | LInt i -> Alloc.box_int st i
  | LBool b -> Alloc.box_bool st b
  | LString s -> Alloc.box_string st s
  | LVoid -> VVoid

let run (st : vm_state) : value =
  let rec loop () =
    let frame =
      match st.frames with
      | f :: _ -> f
      | [] -> Error.vm "0" "run called with no active frame"
    in
    let code = frame.method_info.code in
    if frame.pc < 0 || frame.pc >= Array.length code 
      then Error.vm "0" "pc out of bounds %d" frame.pc;
    let instr = code.(frame.pc) in
    frame.pc <- frame.pc + 1;
    match instr.op with
    | OP_POP ->
      ignore (Stack.pop_val st);
      loop ()

    | OP_CONST ->
      (match instr.arg with
      | IntArg idx ->
        Stack.push_val st (const_of_value st st.ir.consts.(idx))
      | _ ->
        Error.vm "0" "CONST missing IntArg");
      loop ()

    | OP_TRUE ->
      Stack.push_val st (Alloc.box_bool st true);
      loop ()

    | OP_FALSE ->
      Stack.push_val st (Alloc.box_bool st false);
      loop ()

    | OP_VOID ->
      Stack.push_val st VVoid;
      loop ()

    | OP_GET_LOCAL ->
      (match instr.arg with
        | IntArg slot ->
          let v = Stack.get_local st slot in
          Stack.push_val st v
        | _ ->
          Error.vm "0" "GET_LOCAL missing IntArg");
      loop ()

    | OP_SET_LOCAL ->
      (match instr.arg with
      | IntArg slot ->
        let v = Stack.pop_val st in
        Stack.set_local st slot v;
        Stack.push_val st v
      | _ ->
        Error.vm "0" "SET_LOCAL missing IntArg");
      loop ()

    | OP_GET_SELF ->
      let self =
        match st.frames with
        | f :: _ -> f.self_obj
        | [] -> Error.vm "0" "GET_SELF no frame"
      in
      Stack.push_val st (VObj self);
      loop ()

    | OP_GET_ATTR ->
      (match instr.arg with
      | IntArg off ->
        let recv =
          match Stack.pop_val st with
          | VObj o -> o
          | _ -> Error.vm "0" "GET_ATTR on non object"
        in
        Stack.push_val st (Array.get recv.fields off)
      | _ ->
        Error.vm "0" "GET_ATTR missing IntArg");
      loop ()

    | OP_SET_ATTR ->
      (match instr.arg with
      | IntArg off ->
        let v = Stack.pop_val st in
        let recv =
          match Stack.pop_val st with
          | VObj o -> o
          | _ -> Error.vm "0" "SET_ATTR on non-object"
        in
        Array.set recv.fields off v;
        Stack.push_val st v
      | _ ->
        Error.vm "0" "SET_ATTR missing IntArg");
      loop ()

    | OP_NEW ->
      (match instr.arg with
      | IntArg cid ->
          (* Allocate only. Codegen will emit CALL __init_<Class> explicitly. *)
          let o = Alloc.allocate_object st cid in
          Stack.push_val st (VObj o);
          loop ()
      | _ ->
          Error.vm "0" "NEW missing IntArg")

    | OP_NEW_SELF_TYPE ->
      (* new SELF_TYPE: allocate + dynamically call the right __init_<dynamic> *)
      let self =
        match st.frames with
        | f :: _ -> f.self_obj
        | [] -> Error.vm "0" "NEW_SELF_TYPE no frame"
      in
      let cid = self.class_id in
      let o = Alloc.allocate_object st cid in
      let cls = st.ir.classes.(cid) in
      let init_name = "__init_" ^ cls.name in
      let rec find_init i =
        if i >= Array.length st.ir.methods then
          Error.vm "0" "missing constructor for %s" cls.name
        else if st.ir.methods.(i).name = init_name then i
        else find_init (i + 1)
      in
      let init_mid = find_init 0 in
      (* Do NOT push (VObj o) here; __init_* will RETURN self and that becomes the value. *)
      Stack.push_frame st o init_mid [];
      loop ()


    | OP_JUMP ->
      Error.vm "0" "jmp not implemented yet";
      loop ()

    | OP_JUMP_IF_FALSE ->
      Error.vm "0" "jmp if false not implemented yet";
      loop ()

    | OP_LOOP ->
      Error.vm "0" "loop not implemented yet";
      loop ()

  | OP_ADD ->
      let rhs = Stack.pop_val st in
      let lhs = Stack.pop_val st in
      Printf.printf "ADD lhs=%s rhs=%s\n%!"
        (Runtime.string_of_value lhs)
        (Runtime.string_of_value rhs);
      let rhs_i = expect_int rhs in
      let lhs_i = expect_int lhs in
      Stack.push_val st (Alloc.box_int st (lhs_i + rhs_i));
      loop ()

    | OP_SUB ->
      let rhs = expect_int (Stack.pop_val st) in
      let lhs = expect_int (Stack.pop_val st) in
      Stack.push_val st (Alloc.box_int st (to_int32 (lhs - rhs)));
      loop ()

    | OP_MUL ->
      let rhs = expect_int (Stack.pop_val st) in
      let lhs = expect_int (Stack.pop_val st) in
      Stack.push_val st (Alloc.box_int st (to_int32 (lhs * rhs)));
      loop ()

    | OP_DIV ->
      let rhs = expect_int (Stack.pop_val st) in
      let lhs = expect_int (Stack.pop_val st) in
      if rhs = 0 then Error.vm "0" "division by zero";
      Stack.push_val st (Alloc.box_int st (to_int32 (lhs / rhs)));
      loop ()

    | OP_NEG ->
      let i = expect_int (Stack.pop_val st) in
      Stack.push_val st (Alloc.box_int st (to_int32 (-i)));
      loop ()

    | OP_NOT ->
      let b = expect_bool (Stack.pop_val st) in
      Stack.push_val st (Alloc.box_bool st (not b));
      loop ()

    | OP_EQUAL ->
      Error.vm "0" "equal not implemented yet";
      loop ()

    | OP_LESS ->
      Error.vm "0" "less not implemented yet";
      loop ()

    | OP_LESS_EQUAL ->
      Error.vm "0" "less than or equal to not implemented yet";
      loop ()

    | OP_ISVOID ->
      let v = Stack.pop_val st in
      (match v with
      | VVoid -> Stack.push_val st (Alloc.box_bool st true)
      | _ -> Stack.push_val st (Alloc.box_bool st false));
      loop ()

    | OP_CALL ->
      (match instr.arg with
      | IntArg mid ->
        let m = st.ir.methods.(mid) in
        let nargs = m.n_formals in

        let rec pop_args acc n =
          if n = 0 then acc
          else pop_args (Stack.pop_val st :: acc) (n - 1)
        in
        let args = pop_args [] nargs in

        let recv =
          match Stack.pop_val st with
          | VObj o -> o
          | _ -> Error.vm "0" "CALL without object receiver"
        in

        Stack.push_frame st recv mid args;
        loop ()
      | _ -> Error.vm "0" "CALL missing IntArg")

    | OP_DISPATCH ->
      (match instr.arg with
      | IntArg slot ->
          (* pop args FIRST *)
          let cls = 
            match Stack.peek_val st with        (* receiver is BELOW args, so peek *)
            | VObj o -> st.ir.classes.(o.class_id)
            | _ -> Error.vm "0" "dispatch on non-object"
          in

          let nargs = (st.ir.methods.(cls.dispatch.(slot))).n_formals in
          let rec pop_args acc n =
            if n = 0 then acc
            else pop_args (Stack.pop_val st :: acc) (n-1)
          in
          let args = pop_args [] nargs in

          (* now pop receiver LAST *)
          let recv =
            match Stack.pop_val st with
            | VObj o -> o
            | _ -> Error.vm "0" "dispatch missing receiver"
          in

          (* dispatch actual method *)
          let mid = cls.dispatch.(slot) in
          Stack.push_frame st recv mid args;
          loop ()
      | _ -> Error.vm "0" "DISPATCH missing IntArg")




    | OP_STATIC_DISPATCH ->
      (match instr.arg with
      | IntArg mid ->
          let m = st.ir.methods.(mid) in
          let nargs = m.n_formals in

          (* pop args FIRST *)
          let rec pop_args acc n =
            if n = 0 then acc
            else pop_args (Stack.pop_val st :: acc) (n-1)
          in
          let args = pop_args [] nargs in

          (* NOW pop receiver LAST (IMPORTANT) *)
          let recv =
            match Stack.pop_val st with
            | VObj o -> o
            | _ -> Error.vm "0" "STATIC_DISPATCH missing receiver"
          in

          Stack.push_frame st recv mid args;
          loop ()
      | _ -> Error.vm "0" "STATIC_DISPATCH missing IntArg")


    | OP_RETURN ->
      let frame =
        match st.frames with
        | f :: _ -> f
        | [] -> Error.vm "0" "RETURN with no frame"
      in      
      (match Builtin.maybe_handle_builtin st frame with
      | Some v ->
        ignore (Stack.pop_frame st);
        Stack.push_val st v;
        if st.frames = [] then v else loop ()
      | None ->
        let ret = Stack.pop_val st in
        match st.frames with
        | _current :: caller :: rest ->
          ignore (Stack.pop_frame st);
          st.frames <- caller :: rest;
          Stack.push_val st ret;
          loop ()
        | [_] ->
          ignore (Stack.pop_frame st);
          Stack.push_val st ret;
          ret
        | [] ->
          Error.vm "0" "RETURN with no frame")

    | OP_NOP ->
      Error.vm "0" "no op not implemented yet";
      loop ()
  in
  loop ()