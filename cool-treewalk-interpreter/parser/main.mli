type token =
  | STRING of (
# 53 "main.mly"
        string * string
# 6 "main.mli"
)
  | IDENTIFIER of (
# 53 "main.mly"
        string * string
# 11 "main.mli"
)
  | TYPE of (
# 53 "main.mly"
        string * string
# 16 "main.mli"
)
  | INTEGER of (
# 53 "main.mly"
        string * string
# 21 "main.mli"
)
  | PLUS of (
# 54 "main.mly"
        string
# 26 "main.mli"
)
  | MINUS of (
# 54 "main.mly"
        string
# 31 "main.mli"
)
  | TIMES of (
# 54 "main.mly"
        string
# 36 "main.mli"
)
  | DIVIDE of (
# 54 "main.mly"
        string
# 41 "main.mli"
)
  | EQUALS of (
# 54 "main.mly"
        string
# 46 "main.mli"
)
  | AT of (
# 55 "main.mly"
        string
# 51 "main.mli"
)
  | LT of (
# 55 "main.mly"
        string
# 56 "main.mli"
)
  | SEMI of (
# 55 "main.mly"
        string
# 61 "main.mli"
)
  | TILDE of (
# 55 "main.mly"
        string
# 66 "main.mli"
)
  | DOT of (
# 55 "main.mly"
        string
# 71 "main.mli"
)
  | COMMA of (
# 55 "main.mly"
        string
# 76 "main.mli"
)
  | COLON of (
# 55 "main.mly"
        string
# 81 "main.mli"
)
  | RBRACE of (
# 56 "main.mly"
        string
# 86 "main.mli"
)
  | LBRACE of (
# 56 "main.mly"
        string
# 91 "main.mli"
)
  | RPAREN of (
# 56 "main.mly"
        string
# 96 "main.mli"
)
  | LPAREN of (
# 56 "main.mly"
        string
# 101 "main.mli"
)
  | LE of (
# 56 "main.mly"
        string
# 106 "main.mli"
)
  | LARROW of (
# 57 "main.mly"
        string
# 111 "main.mli"
)
  | RARROW of (
# 57 "main.mly"
        string
# 116 "main.mli"
)
  | TRUE of (
# 57 "main.mly"
        string
# 121 "main.mli"
)
  | FALSE of (
# 57 "main.mly"
        string
# 126 "main.mli"
)
  | NEW of (
# 57 "main.mly"
        string
# 131 "main.mli"
)
  | NOT of (
# 57 "main.mly"
        string
# 136 "main.mli"
)
  | LET of (
# 58 "main.mly"
        string
# 141 "main.mli"
)
  | ELSE of (
# 58 "main.mly"
        string
# 146 "main.mli"
)
  | CLASS of (
# 58 "main.mly"
        string
# 151 "main.mli"
)
  | CASE of (
# 58 "main.mly"
        string
# 156 "main.mli"
)
  | ESAC of (
# 58 "main.mly"
        string
# 161 "main.mli"
)
  | INHERITS of (
# 58 "main.mly"
        string
# 166 "main.mli"
)
  | LOOP of (
# 58 "main.mly"
        string
# 171 "main.mli"
)
  | POOL of (
# 59 "main.mly"
        string
# 176 "main.mli"
)
  | ISVOID of (
# 59 "main.mly"
        string
# 181 "main.mli"
)
  | OF of (
# 59 "main.mly"
        string
# 186 "main.mli"
)
  | IN of (
# 59 "main.mly"
        string
# 191 "main.mli"
)
  | IF of (
# 59 "main.mly"
        string
# 196 "main.mli"
)
  | FI of (
# 59 "main.mly"
        string
# 201 "main.mli"
)
  | WHILE of (
# 59 "main.mly"
        string
# 206 "main.mli"
)
  | THEN of (
# 59 "main.mly"
        string
# 211 "main.mli"
)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> program
