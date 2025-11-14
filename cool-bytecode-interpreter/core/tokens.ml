(*
@author Trey Rubino
@date 11/13/2025
*)

type token = 
  | INTEGER of int
  | STRING of string
  | IDENTIFIER of string
  | TYPE of string
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | AT
  | LT
  | SEMI
  | EQUALS
  | TILDE
  | DOT
  | COMMA
  | COLON
  | RBRACE
  | LBRACE
  | RPAREN    
  | LPAREN
  | LE
  | LARROW
  | RARROW
  | TRUE
  | FALSE
  | NEW
  | NOT
  | LET
  | ELSE
  | CLASS
  | CASE 
  | ESAC
  | INHERITS
  | LOOP
  | POOL
  | ISVOID
  | OF
  | IN
  | IF
  | FI
  | WHILE
  | THEN