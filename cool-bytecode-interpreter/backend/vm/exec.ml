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

let const_of_value = function 
  | LInt i -> VInt i
  | LBool b -> VBool b
  | LString s -> VString s
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
        Stack.push_val st (const_of_value st.ir.consts.(idx))
      | _ ->
        Error.vm "0" "CONST missing IntArg");
      loop ()

    | OP_TRUE ->
      Stack.push_val st (VBool true);
      loop ()

    | OP_FALSE ->
      Stack.push_val st (VBool false);
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
          Error.vm "0" "GET_LOCAL missing IntArg"
      );
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
        let v = Alloc.allocate_and_init st cid in
        Stack.push_val st v;
        loop ()
      | _ ->
        Error.vm "0" "NEW missing IntArg");

    | OP_NEW_SELF_TYPE ->
      let recv =
        match Stack.pop_val st with
        | VObj o -> o
        | _ -> Error.vm "0" "NEW_SELF_TYPE on non-object"
      in
      let cid = recv.class_id in
      let v = Alloc.allocate_and_init st cid in
      Stack.push_val st v;
      loop ()

    | OP_CALL ->
      (match instr.arg with
      | IntArg mid ->
        let m = st.ir.methods.(mid) in
        let nargs = m.n_formals in
        let rec pop_args acc n =
          if n = 0 then acc
          else let v = Stack.pop_val st in pop_args (v :: acc) (n - 1)
        in
        let args = pop_args [] nargs in
        let recv =
          match Stack.pop_val st with
          | VObj o -> o
          | _ -> Error.vm "0" "CALL without object receiver"
        in
        Stack.push_frame st recv mid args;
        loop ()
      | _ ->
        Error.vm "0" "CALL missing IntArg")

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
      (match lhs, rhs with
      | VInt i1, VInt i2 -> 
        Stack.push_val st (VInt (to_int32 (i1 + i2)))
      | _ -> Error.vm "0" "addition requires two integers");
      loop ()

    | OP_SUB ->
      let rhs = Stack.pop_val st in
      let lhs = Stack.pop_val st in
      (match lhs, rhs with
      | VInt i1, VInt i2 -> 
        Stack.push_val st (VInt (to_int32 (i1 - i2)))
      | _ -> Error.vm "0" "subtraction requires two integers");
      loop ()

    | OP_MUL ->
      let rhs = Stack.pop_val st in
      let lhs = Stack.pop_val st in
      (match lhs, rhs with
      | VInt i1, VInt i2 -> 
        Stack.push_val st (VInt (to_int32 (i1 * i2)))
      | _ -> Error.vm "0" "multiplication requires two integers");
      loop ()

    | OP_DIV ->
      let rhs = Stack.pop_val st in
      let lhs = Stack.pop_val st in
      (match lhs, rhs with
      | VInt i1, VInt i2 -> 
        if i2 = 0 then Error.vm "0" "division by zero"
        Stack.push_val st (VInt (to_int32 (i1 * i2)))
      | _ -> Error.vm "0" "divison requires two integers");
      loop ()

    | OP_NEG ->
      let v = Stack.pop_val st in
      (match v with
      | VInt i -> Stack.push_val st (VInt (to_int32 (-i)))
      | _ -> Error.vm "0" "negation requires one integer");
      loop ()

    | OP_NOT ->
      let v = Stack.pop_val st in
      (match v with
      | VBool b -> Stack.push_val st (VBool (not b))
      | _ -> Error.vm "0" "not requires boolean");
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
      Error.vm "0" "isvoid not implemented yet";
      loop ()

    | OP_DISPATCH ->
      (match instr.arg with
      | IntArg slot ->
        let m = st.ir.methods.(slot) in
        let nargs = m.n_formals in

        let rec pop_args acc n =
          if n = 0 then acc
          else pop_args (Stack.pop_val st :: acc) (n - 1)
        in
        let args = pop_args [] nargs in
        let recv =
          match Stack.pop_val st with
          | VObj o -> o
          | VString s -> Printf.printf "string\n"; exit 1
          | _ -> Error.vm "0" "DISPATCH on non-object receiver"
        in
        let cls = st.ir.classes.(recv.class_id) in
        let real_method_id =
          try cls.dispatch.(slot)
          with _ -> Error.vm "0" "invalid dispatch slot"
        in
        Stack.push_frame st recv real_method_id args;
        loop ()
      | _ ->
        Error.vm "0" "DISPATCH missing IntArg")

    | OP_STATIC_DISPATCH ->
      Error.vm "0" "static dispatch not implemented yet";
      loop ()

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