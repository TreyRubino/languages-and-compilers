(*
@author Trey Rubino
@date 11/13/2025
*)

type token = 
  | INTEGER of (string * string)
  | STRING of (string * string)
  | IDENTIFIER of (string * string)
  | TYPE of (string * string)
  | PLUS of string
  | MINUS of string
  | TIMES of string
  | DIVIDE of string
  | AT of string
  | LT of string
  | SEMI of string
  | EQUALS of string
  | TILDE of string
  | DOT of string
  | COMMA of string
  | COLON of string
  | RBRACE of string
  | LBRACE of string
  | RPAREN of string   
  | LPAREN of string
  | LE of string
  | LARROW of string
  | RARROW of string
  | TRUE of string
  | FALSE of string
  | NEW of string
  | NOT of string
  | LET of string
  | ELSE of string
  | CLASS of string
  | CASE of string
  | ESAC of string
  | INHERITS of string
  | LOOP of string
  | POOL of string
  | ISVOID of string
  | OF of string
  | IN of string
  | IF of string
  | FI of string
  | WHILE of string
  | THEN of string
  | EOF