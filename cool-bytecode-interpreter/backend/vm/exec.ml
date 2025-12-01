(*
@author Trey Rubino
@date 11/30/2025
*)

open Runtime 
open Alloc 
open Bytecode
open Stack
open Ir
open Error

let make_frame (recv : obj) (m : method_info) (ret_pc : int) : frame = 
  {
    method_id = m.class_id;
    code = m.code;
    pc = 0;
    locals = Array.make (m.n_locals + m.n_formals + 1) VVoid;
    self = recv; 
    ret_pc;
  }

let invoke (st : vm_state) (mid : int) (args : value list) : value =
  let m = st.ir.methods(mid) in
  let cls = st.ir.classes.(m.class_id) in

  let recv = 
    match args with
    | VObj o :: _ -> o
    | _ -> Error.vm "0" "method invoked without object receiver"
  in

  (* new frame *)
  let frame = make_frame m (-1) in
  list.iteri (fun i v -> 
    frame.locals.(i) <- v
  ) args;

  st.frames <- frame :: st.frames;
  let result = run st in
  st.frames <- List.tl st.frames;
  result

and run (st : vm_state) : value =
  let frame = 
    match st.frames with
    | f :: _ -> f
    | [] -> Error.vm "0" "run called with no active frame"
  in
  let code = frame.code in

  (* the 'The Big Loop' *)
  let rec loop () = 
    if frame.pc < 0 || frame.pc >= Array.length code then
      Error.vm "0" "pc out of bounds";
    
    let instr = code.(frame.pc) in
    frame.pc <- frame.pc + 1;

    match instr.op with
    | OP_POP

    | OP_CONST -> 
      (match instr.arg with
      | IntArg idx -> 
        Stack.push st(const_of_value st.ir.consts(idx)))
      | _ -> Error.vm "0" "CONST missing IntArg"
      loop ()

    | OP_TRUE
      Stack.push st (VBool true);
      loop ()

    | OP_FALSE
      Stack.push st (VBool false);
      loop ()

    | OP_VOID
      Stack.push st VVoid;
      loop ()

    | OP_GET_LOCAL
      (match instr.arg with
      | IntArg slot -> 
        let v = Stack.get_local st slot in
        Stack.push st v
      | _ -> Error.vm "0" "GET_LOCAL missing IntArg");
      loop ();

    | OP_SET_LOCAL
      (match instr.arg with
      | IntArg slot -> 
        let v = Stack.pop st in
        Stack.set_local st slot v;
        Stack.push st v
      | _ -> Error.vm "0" "SET_LOCAL missing IntArg");
      loop ();

    | OP_GET_SELF
      let self = 
        match st.frames with 
        | f :: _ -> f.self
        | [] -> Error.vm "0" "GET_SELF no frame"
      in
      Stack.push st (VObj self);
      loop ()

    | OP_GET_ATTR
      (match instr.arg with
      | IntArg off -> 
        let recv =
          match Stack.pop st with
          | VObj o -> o
          | _ -> Error.vm "0" "GET_ATTR on non object"
        in
        Stack.push st (Array.get recv.fields off)
      | _ -> Error.vm "0" "GET_ATTR missing IntArg");
      loop()
      
    | OP_SET_ATTR ->
      (match instr.arg with
      | IntArg off ->
        let v = Stack.pop st in
        let recv =
          match Stack.pop st with
          | VObj o -> o
          | _ -> Error.vm "0" "SET_ATTR on non-object"
        in
        Array.set recv.fields off v;
        Stack.push st v
      | _ -> Error.vm "0" "SET_ATTR missing IntArg");
      loop ()

    | OP_NEW
      (match instr.arg with
      | IntArg cid -> 
        let v = Alloc.allocate_and_init st cid in
        stack.push st v
      | _ -> Error.vm "0" " NEW missing IntArg");
      loop ()

    | OP_NEW_SELF_TYPE
      let recv = 
        match Stack.pop st with
        | VObj o -> o
        | _ -> Error.vm "0" "NEW_SELF_TYPE on non object"
      in
      let cid = recv.class_id in
      let v = Alloc.allocate_and_init st cid in
      Stack.push st v;
      loop ()

    | OP_CALL
      (match instr.arg with
      | IntArg mid -> 
        Error.vm "0" "CALL not implemented yet"
      | _ -> Error.vm "0" "CALL missing IntArg")
    | OP_JUMP
      
    | OP_JUMP_IF_FALSE
    | OP_LOOP
    | OP_ADD
    | OP_SUB
    | OP_MUL
    | OP_DIV
    | OP_NEG
    | OP_NOT
    | OP_EQUAL
    | OP_LESS
    | OP_LESS_EQUAL
    | OP_ISVOID
    | OP_DISPATCH ->
        Error.vm "0" "dynamic dispatch not implemented yet"

    | OP_STATIC_DISPATCH ->
        Error.vm "0" "static dispatch not implemented yet"
    | OP_RETURN
    | OP_NOP




