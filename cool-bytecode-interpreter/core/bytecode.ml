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
  | OP_CASE_ABORT
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
  | OP_NOP

  let string_of_op = function
  | OP_POP -> "OP_POP"
  | OP_CONST -> "OP_CONST"
  | OP_TRUE -> "OP_TRUE"
  | OP_FALSE -> "OP_FALSE"
  | OP_VOID -> "OP_VOID"
  | OP_GET_LOCAL -> "OP_GET_LOCAL"
  | OP_SET_LOCAL -> "OP_SET_LOCAL"
  | OP_GET_SELF -> "OP_GET_SELF"
  | OP_GET_ATTR -> "OP_GET_ATTR"
  | OP_SET_ATTR -> "OP_SET_ATTR"
  | OP_NEW -> "OP_NEW"
  | OP_NEW_SELF_TYPE -> "OP_NEW_SELF_TYPE"
  | OP_CALL -> "OP_CALL"
  | OP_JUMP -> "OP_JUMP"
  | OP_JUMP_IF_FALSE -> "OP_JUMP_IF_FALSE"
  | OP_CASE_ABORT -> "OP_CASE_ABORT"
  | OP_ADD -> "OP_ADD"
  | OP_SUB -> "OP_SUB"
  | OP_MUL -> "OP_MUL"
  | OP_DIV -> "OP_DIV"
  | OP_NEG -> "OP_NEG"
  | OP_NOT -> "OP_NOT"
  | OP_EQUAL -> "OP_EQUAL"
  | OP_LESS -> "OP_LESS"
  | OP_LESS_EQUAL -> "OP_LESS_EQUAL"
  | OP_ISVOID -> "OP_ISVOID"
  | OP_DISPATCH -> "OP_DISPATCH"
  | OP_STATIC_DISPATCH -> "OP_STATIC_DISPATCH"
  | OP_RETURN -> "OP_RETURN"
  | OP_NOP -> "OP_NOP"

type operand = 
  | NoArg
  | IntArg of int
  | OffsetArg of int

type instruction = {
  op : opcode;
  arg: operand;
}

type program = instruction array