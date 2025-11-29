(*
@author Trey Rubino
@date 11/15/2025
*)

type opcode =
  | OP_POP
  | OP_CONST
  | OP_TRUE
  | OP_FALSE
  | OP_VOID
  | OP_GET_LOCAL
  | OP_SET_LOCAL
  | OP_GET_SELF
  | OP_GET_ATTR
  | OP_SET_ATTR
  | OP_NEW
  | OP_NEW_SELF_TYPE
  | OP_CALL
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
  | OP_DISPATCH
  | OP_STATIC_DISPATCH
  | OP_RETURN
  | OP_CASE_INIT
  | OP_CASE_BRANCH
  | OP_CASE_END
  | OP_NOP

type operand = 
  | NoArg
  | IntArg of int
  | OffestArg of int

type instruction = {
  op : opcode;
  arg: operand;
}

type program = instruction array