type token =
  | STRING of (
# 53 "main.mly"
        string * string
# 6 "main.ml"
)
  | IDENTIFIER of (
# 53 "main.mly"
        string * string
# 11 "main.ml"
)
  | TYPE of (
# 53 "main.mly"
        string * string
# 16 "main.ml"
)
  | INTEGER of (
# 53 "main.mly"
        string * string
# 21 "main.ml"
)
  | PLUS of (
# 54 "main.mly"
        string
# 26 "main.ml"
)
  | MINUS of (
# 54 "main.mly"
        string
# 31 "main.ml"
)
  | TIMES of (
# 54 "main.mly"
        string
# 36 "main.ml"
)
  | DIVIDE of (
# 54 "main.mly"
        string
# 41 "main.ml"
)
  | EQUALS of (
# 54 "main.mly"
        string
# 46 "main.ml"
)
  | AT of (
# 55 "main.mly"
        string
# 51 "main.ml"
)
  | LT of (
# 55 "main.mly"
        string
# 56 "main.ml"
)
  | SEMI of (
# 55 "main.mly"
        string
# 61 "main.ml"
)
  | TILDE of (
# 55 "main.mly"
        string
# 66 "main.ml"
)
  | DOT of (
# 55 "main.mly"
        string
# 71 "main.ml"
)
  | COMMA of (
# 55 "main.mly"
        string
# 76 "main.ml"
)
  | COLON of (
# 55 "main.mly"
        string
# 81 "main.ml"
)
  | RBRACE of (
# 56 "main.mly"
        string
# 86 "main.ml"
)
  | LBRACE of (
# 56 "main.mly"
        string
# 91 "main.ml"
)
  | RPAREN of (
# 56 "main.mly"
        string
# 96 "main.ml"
)
  | LPAREN of (
# 56 "main.mly"
        string
# 101 "main.ml"
)
  | LE of (
# 56 "main.mly"
        string
# 106 "main.ml"
)
  | LARROW of (
# 57 "main.mly"
        string
# 111 "main.ml"
)
  | RARROW of (
# 57 "main.mly"
        string
# 116 "main.ml"
)
  | TRUE of (
# 57 "main.mly"
        string
# 121 "main.ml"
)
  | FALSE of (
# 57 "main.mly"
        string
# 126 "main.ml"
)
  | NEW of (
# 57 "main.mly"
        string
# 131 "main.ml"
)
  | NOT of (
# 57 "main.mly"
        string
# 136 "main.ml"
)
  | LET of (
# 58 "main.mly"
        string
# 141 "main.ml"
)
  | ELSE of (
# 58 "main.mly"
        string
# 146 "main.ml"
)
  | CLASS of (
# 58 "main.mly"
        string
# 151 "main.ml"
)
  | CASE of (
# 58 "main.mly"
        string
# 156 "main.ml"
)
  | ESAC of (
# 58 "main.mly"
        string
# 161 "main.ml"
)
  | INHERITS of (
# 58 "main.mly"
        string
# 166 "main.ml"
)
  | LOOP of (
# 58 "main.mly"
        string
# 171 "main.ml"
)
  | POOL of (
# 59 "main.mly"
        string
# 176 "main.ml"
)
  | ISVOID of (
# 59 "main.mly"
        string
# 181 "main.ml"
)
  | OF of (
# 59 "main.mly"
        string
# 186 "main.ml"
)
  | IN of (
# 59 "main.mly"
        string
# 191 "main.ml"
)
  | IF of (
# 59 "main.mly"
        string
# 196 "main.ml"
)
  | FI of (
# 59 "main.mly"
        string
# 201 "main.ml"
)
  | WHILE of (
# 59 "main.mly"
        string
# 206 "main.ml"
)
  | THEN of (
# 59 "main.mly"
        string
# 211 "main.ml"
)
  | EOF

open Parsing
let _ = parse_error;;
# 2 "main.mly"
(* Trey Rubino -- COOL Parser -- Dr. Schwesinger *)

open Printf

type identifier = string * string
type structure =
  | ClassNoInherits of identifier * (feature list)
  | ClassInherits of identifier * identifier * (feature list)
and feature = 
  | AttributeNoInit of identifier * identifier
  | AttributeInit of identifier * identifier * expr
  | Method of identifier * (formal list) * identifier * expr
and formal = identifier * identifier 
and expr_internal = 
  | Assign of identifier * expr
  | DynamicDispatch of expr * identifier * expr list
  | StaticDispatch of expr * identifier * identifier * expr list 
  | SelfDispatch of identifier * expr list
  | If of expr * expr * expr
  | While of expr * expr
  | Let of (let_binding list) * expr
  | Case of expr * (identifier * identifier * expr) list
  | New of identifier
  | Isvoid of expr
  | Plus of expr * expr
  | Minus of expr * expr
  | Times of expr * expr
  | Divide of expr * expr
  | Tilde of expr
  | Lt of expr * expr
  | Le of expr * expr
  | Equals of expr * expr
  | Not of expr
  | Identifier of identifier
  | Integer of string
  | String of string
  | True
  | False
  | Block of expr list
and let_binding =
  | LetBindingNoInit of identifier * identifier
  | LetBindingInit of identifier * identifier * expr
and expr = string * expr_internal

type call_suffix =
  | CallDyn  of identifier * (expr list)                       (* .f(args) *)
  | CallStat of identifier (* TYPE *) * identifier * (expr list)  (* @T.f(args) *)

type program = structure list
# 267 "main.ml"
let yytransl_const = [|
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* STRING *);
  258 (* IDENTIFIER *);
  259 (* TYPE *);
  260 (* INTEGER *);
  261 (* PLUS *);
  262 (* MINUS *);
  263 (* TIMES *);
  264 (* DIVIDE *);
  265 (* EQUALS *);
  266 (* AT *);
  267 (* LT *);
  268 (* SEMI *);
  269 (* TILDE *);
  270 (* DOT *);
  271 (* COMMA *);
  272 (* COLON *);
  273 (* RBRACE *);
  274 (* LBRACE *);
  275 (* RPAREN *);
  276 (* LPAREN *);
  277 (* LE *);
  278 (* LARROW *);
  279 (* RARROW *);
  280 (* TRUE *);
  281 (* FALSE *);
  282 (* NEW *);
  283 (* NOT *);
  284 (* LET *);
  285 (* ELSE *);
  286 (* CLASS *);
  287 (* CASE *);
  288 (* ESAC *);
  289 (* INHERITS *);
  290 (* LOOP *);
  291 (* POOL *);
  292 (* ISVOID *);
  293 (* OF *);
  294 (* IN *);
  295 (* IF *);
  296 (* FI *);
  297 (* WHILE *);
  298 (* THEN *);
    0|]

let yylhs = "\255\255\
\001\000\013\000\013\000\014\000\014\000\015\000\015\000\016\000\
\016\000\016\000\017\000\017\000\017\000\018\000\011\000\011\000\
\011\000\002\000\002\000\003\000\003\000\003\000\003\000\004\000\
\004\000\004\000\005\000\005\000\005\000\006\000\006\000\006\000\
\006\000\007\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\009\000\
\009\000\010\000\010\000\012\000\012\000\021\000\021\000\019\000\
\019\000\022\000\020\000\020\000\000\000"

let yylen = "\002\000\
\001\000\000\000\003\000\005\000\007\000\000\000\003\000\003\000\
\005\000\009\000\000\000\001\000\003\000\003\000\000\000\001\000\
\003\000\003\000\001\000\003\000\003\000\003\000\001\000\003\000\
\003\000\001\000\003\000\003\000\001\000\002\000\002\000\002\000\
\001\000\002\000\007\000\005\000\004\000\005\000\004\000\002\000\
\003\000\003\000\001\000\001\000\001\000\001\000\001\000\000\000\
\002\000\005\000\007\000\003\000\002\000\003\000\005\000\001\000\
\003\000\005\000\002\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\061\000\001\000\000\000\000\000\000\000\
\000\000\000\000\003\000\000\000\000\000\000\000\000\000\000\000\
\000\000\004\000\000\000\000\000\000\000\000\000\000\000\000\000\
\007\000\000\000\000\000\000\000\000\000\000\000\005\000\045\000\
\000\000\044\000\000\000\000\000\000\000\046\000\047\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\009\000\000\000\
\000\000\000\000\029\000\033\000\000\000\014\000\000\000\013\000\
\000\000\000\000\000\000\030\000\000\000\000\000\000\000\040\000\
\031\000\000\000\000\000\000\000\000\000\032\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\034\000\000\000\000\000\000\000\000\000\018\000\000\000\
\042\000\041\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\027\000\028\000\000\000\
\000\000\049\000\000\000\000\000\039\000\052\000\000\000\037\000\
\057\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\017\000\000\000\000\000\038\000\000\000\000\000\036\000\
\000\000\000\000\010\000\055\000\000\000\060\000\000\000\000\000\
\050\000\000\000\035\000\000\000\058\000\051\000"

let yydgoto = "\002\000\
\004\000\085\000\048\000\049\000\050\000\051\000\052\000\053\000\
\082\000\083\000\086\000\062\000\005\000\006\000\013\000\014\000\
\023\000\024\000\067\000\115\000\068\000\116\000"

let yysindex = "\006\000\
\240\254\000\000\018\255\000\000\000\000\022\255\255\254\240\254\
\040\255\041\255\000\000\241\254\043\255\055\255\060\255\076\255\
\079\255\000\000\040\255\040\255\061\255\068\255\067\255\073\255\
\000\000\072\255\002\255\087\255\075\255\079\255\000\000\000\000\
\017\255\000\000\046\255\002\255\002\255\000\000\000\000\089\255\
\046\255\091\255\002\255\046\255\002\255\002\255\000\000\014\255\
\052\255\044\255\000\000\000\000\039\255\000\000\092\255\000\000\
\002\255\002\255\074\255\000\000\085\255\081\255\080\255\000\000\
\000\000\086\255\063\255\099\255\083\255\000\000\082\255\084\255\
\046\255\046\255\046\255\046\255\046\255\046\255\046\255\113\255\
\119\255\000\000\039\255\104\255\108\255\106\255\000\000\002\255\
\000\000\000\000\123\255\002\255\091\255\126\255\002\255\002\255\
\052\255\052\255\052\255\044\255\044\255\000\000\000\000\115\255\
\110\255\000\000\002\255\002\255\000\000\000\000\109\255\000\000\
\000\000\118\255\105\255\127\255\114\255\117\255\142\255\002\255\
\132\255\000\000\002\255\151\255\000\000\126\255\002\255\000\000\
\138\255\137\255\000\000\000\000\139\255\000\000\120\255\002\255\
\000\000\002\255\000\000\140\255\000\000\000\000"

let yyrindex = "\000\000\
\161\000\000\000\000\000\000\000\000\000\000\000\000\000\161\000\
\146\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\145\255\000\000\146\255\146\255\022\255\000\000\000\000\147\255\
\000\000\000\000\000\000\000\000\000\000\145\255\000\000\000\000\
\098\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\032\000\
\068\000\136\255\000\000\000\000\174\255\000\000\000\000\000\000\
\148\255\000\000\098\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\130\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\174\255\000\000\150\255\000\000\000\000\155\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\104\000\140\000\176\000\212\255\250\255\000\000\000\000\000\000\
\000\000\000\000\000\000\148\255\000\000\000\000\254\254\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\148\255\
\000\000\000\000\000\000\000\000\000\000\143\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\148\255\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\229\255\000\000\237\255\242\255\223\255\000\000\000\000\
\094\000\000\000\160\255\096\000\179\000\000\000\056\000\000\000\
\158\000\000\000\097\000\066\000\000\000\000\000"

let yytablesize = 474
let yytable = "\047\000\
\016\000\060\000\032\000\033\000\017\000\034\000\001\000\065\000\
\061\000\063\000\070\000\122\000\054\000\003\000\035\000\069\000\
\009\000\071\000\072\000\036\000\007\000\037\000\073\000\130\000\
\074\000\038\000\039\000\040\000\041\000\042\000\087\000\010\000\
\043\000\008\000\075\000\054\000\057\000\044\000\058\000\140\000\
\045\000\012\000\046\000\015\000\102\000\103\000\032\000\059\000\
\080\000\034\000\078\000\079\000\081\000\097\000\098\000\099\000\
\076\000\077\000\035\000\018\000\061\000\100\000\101\000\036\000\
\112\000\037\000\019\000\117\000\118\000\038\000\039\000\040\000\
\041\000\042\000\025\000\026\000\043\000\020\000\021\000\121\000\
\022\000\044\000\027\000\028\000\045\000\029\000\046\000\030\000\
\031\000\054\000\055\000\064\000\066\000\057\000\084\000\132\000\
\088\000\089\000\090\000\135\000\092\000\091\000\043\000\043\000\
\043\000\043\000\043\000\043\000\043\000\043\000\141\000\043\000\
\043\000\093\000\043\000\104\000\043\000\096\000\043\000\094\000\
\105\000\107\000\108\000\095\000\109\000\111\000\043\000\114\000\
\119\000\120\000\123\000\043\000\043\000\124\000\043\000\043\000\
\125\000\043\000\126\000\043\000\026\000\026\000\127\000\129\000\
\026\000\026\000\026\000\026\000\131\000\026\000\026\000\128\000\
\026\000\133\000\026\000\137\000\026\000\136\000\142\000\139\000\
\002\000\138\000\006\000\011\000\026\000\012\000\015\000\056\000\
\016\000\026\000\026\000\053\000\026\000\026\000\059\000\026\000\
\106\000\026\000\048\000\048\000\048\000\048\000\048\000\110\000\
\048\000\048\000\011\000\056\000\048\000\113\000\048\000\134\000\
\048\000\000\000\048\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\048\000\000\000\000\000\000\000\000\000\048\000\
\048\000\000\000\048\000\048\000\000\000\048\000\000\000\048\000\
\024\000\024\000\000\000\000\000\024\000\024\000\024\000\024\000\
\000\000\024\000\024\000\000\000\024\000\000\000\024\000\000\000\
\024\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\024\000\000\000\000\000\000\000\000\000\024\000\024\000\000\000\
\024\000\024\000\000\000\024\000\000\000\024\000\025\000\025\000\
\000\000\000\000\025\000\025\000\025\000\025\000\000\000\025\000\
\025\000\000\000\025\000\000\000\025\000\000\000\025\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\025\000\000\000\
\000\000\000\000\000\000\025\000\025\000\000\000\025\000\025\000\
\000\000\025\000\000\000\025\000\019\000\019\000\019\000\019\000\
\000\000\019\000\000\000\019\000\000\000\019\000\019\000\000\000\
\019\000\000\000\019\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\019\000\000\000\000\000\000\000\
\000\000\019\000\019\000\000\000\019\000\019\000\000\000\019\000\
\000\000\019\000\023\000\023\000\023\000\023\000\023\000\023\000\
\000\000\023\000\023\000\000\000\023\000\000\000\023\000\000\000\
\023\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\023\000\000\000\000\000\000\000\000\000\023\000\023\000\000\000\
\023\000\023\000\000\000\023\000\000\000\023\000\022\000\022\000\
\022\000\022\000\022\000\022\000\000\000\022\000\022\000\000\000\
\022\000\000\000\022\000\000\000\022\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\022\000\000\000\000\000\000\000\
\000\000\022\000\022\000\000\000\022\000\022\000\000\000\022\000\
\000\000\022\000\020\000\020\000\020\000\020\000\020\000\020\000\
\000\000\020\000\020\000\000\000\020\000\000\000\020\000\000\000\
\020\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\020\000\000\000\000\000\000\000\000\000\020\000\020\000\000\000\
\020\000\020\000\000\000\020\000\000\000\020\000\021\000\021\000\
\021\000\021\000\021\000\021\000\000\000\021\000\021\000\000\000\
\021\000\000\000\021\000\000\000\021\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\021\000\000\000\000\000\000\000\
\000\000\021\000\021\000\000\000\021\000\021\000\000\000\021\000\
\000\000\021\000"

let yycheck = "\027\000\
\016\001\035\000\001\001\002\001\020\001\004\001\001\000\041\000\
\036\000\037\000\044\000\108\000\015\001\030\001\013\001\043\000\
\018\001\045\000\046\000\018\001\003\001\020\001\009\001\120\000\
\011\001\024\001\025\001\026\001\027\001\028\001\058\000\033\001\
\031\001\012\001\021\001\038\001\020\001\036\001\022\001\136\000\
\039\001\002\001\041\001\003\001\078\000\079\000\001\001\002\001\
\010\001\004\001\007\001\008\001\014\001\073\000\074\000\075\000\
\005\001\006\001\013\001\017\001\088\000\076\000\077\000\018\001\
\092\000\020\001\012\001\095\000\096\000\024\001\025\001\026\001\
\027\001\028\001\019\000\020\000\031\001\018\001\003\001\107\000\
\002\001\036\001\022\001\016\001\039\001\019\001\041\001\015\001\
\017\001\003\001\016\001\003\001\002\001\020\001\003\001\123\000\
\012\001\017\001\019\001\127\000\038\001\016\001\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\138\000\014\001\
\015\001\015\001\017\001\003\001\019\001\034\001\021\001\037\001\
\002\001\018\001\015\001\042\001\019\001\003\001\029\001\002\001\
\014\001\020\001\022\001\034\001\035\001\016\001\037\001\038\001\
\032\001\040\001\012\001\042\001\005\001\006\001\029\001\002\001\
\009\001\010\001\011\001\012\001\017\001\014\001\015\001\035\001\
\017\001\003\001\019\001\019\001\021\001\020\001\019\001\040\001\
\000\000\023\001\017\001\019\001\029\001\019\001\019\001\038\001\
\019\001\034\001\035\001\017\001\037\001\038\001\032\001\040\001\
\083\000\042\001\005\001\006\001\007\001\008\001\009\001\088\000\
\011\001\012\001\008\000\030\000\015\001\093\000\017\001\126\000\
\019\001\255\255\021\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\029\001\255\255\255\255\255\255\255\255\034\001\
\035\001\255\255\037\001\038\001\255\255\040\001\255\255\042\001\
\005\001\006\001\255\255\255\255\009\001\010\001\011\001\012\001\
\255\255\014\001\015\001\255\255\017\001\255\255\019\001\255\255\
\021\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\029\001\255\255\255\255\255\255\255\255\034\001\035\001\255\255\
\037\001\038\001\255\255\040\001\255\255\042\001\005\001\006\001\
\255\255\255\255\009\001\010\001\011\001\012\001\255\255\014\001\
\015\001\255\255\017\001\255\255\019\001\255\255\021\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\029\001\255\255\
\255\255\255\255\255\255\034\001\035\001\255\255\037\001\038\001\
\255\255\040\001\255\255\042\001\005\001\006\001\007\001\008\001\
\255\255\010\001\255\255\012\001\255\255\014\001\015\001\255\255\
\017\001\255\255\019\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\029\001\255\255\255\255\255\255\
\255\255\034\001\035\001\255\255\037\001\038\001\255\255\040\001\
\255\255\042\001\007\001\008\001\009\001\010\001\011\001\012\001\
\255\255\014\001\015\001\255\255\017\001\255\255\019\001\255\255\
\021\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\029\001\255\255\255\255\255\255\255\255\034\001\035\001\255\255\
\037\001\038\001\255\255\040\001\255\255\042\001\007\001\008\001\
\009\001\010\001\011\001\012\001\255\255\014\001\015\001\255\255\
\017\001\255\255\019\001\255\255\021\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\029\001\255\255\255\255\255\255\
\255\255\034\001\035\001\255\255\037\001\038\001\255\255\040\001\
\255\255\042\001\007\001\008\001\009\001\010\001\011\001\012\001\
\255\255\014\001\015\001\255\255\017\001\255\255\019\001\255\255\
\021\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\029\001\255\255\255\255\255\255\255\255\034\001\035\001\255\255\
\037\001\038\001\255\255\040\001\255\255\042\001\007\001\008\001\
\009\001\010\001\011\001\012\001\255\255\014\001\015\001\255\255\
\017\001\255\255\019\001\255\255\021\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\029\001\255\255\255\255\255\255\
\255\255\034\001\035\001\255\255\037\001\038\001\255\255\040\001\
\255\255\042\001"

let yynames_const = "\
  EOF\000\
  "

let yynames_block = "\
  STRING\000\
  IDENTIFIER\000\
  TYPE\000\
  INTEGER\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  EQUALS\000\
  AT\000\
  LT\000\
  SEMI\000\
  TILDE\000\
  DOT\000\
  COMMA\000\
  COLON\000\
  RBRACE\000\
  LBRACE\000\
  RPAREN\000\
  LPAREN\000\
  LE\000\
  LARROW\000\
  RARROW\000\
  TRUE\000\
  FALSE\000\
  NEW\000\
  NOT\000\
  LET\000\
  ELSE\000\
  CLASS\000\
  CASE\000\
  ESAC\000\
  INHERITS\000\
  LOOP\000\
  POOL\000\
  ISVOID\000\
  OF\000\
  IN\000\
  IF\000\
  FI\000\
  WHILE\000\
  THEN\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'class_list) in
    Obj.repr(
# 80 "main.mly"
                                                ( _1 )
# 588 "main.ml"
               : program))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "main.mly"
                                                ( [] )
# 594 "main.ml"
               : 'class_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'structure) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'class_list) in
    Obj.repr(
# 85 "main.mly"
                                                ( _1 :: _3 )
# 603 "main.ml"
               : 'class_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string * string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'feature_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 89 "main.mly"
                                                          ( ClassNoInherits(_2, _4) )
# 614 "main.ml"
               : 'structure))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string * string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string * string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'feature_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 90 "main.mly"
                                                          ( ClassInherits(_2, _4, _6) )
# 627 "main.ml"
               : 'structure))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "main.mly"
                                                ( [] )
# 633 "main.ml"
               : 'feature_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'feature) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'feature_list) in
    Obj.repr(
# 95 "main.mly"
                                                ( _1 :: _3 )
# 642 "main.ml"
               : 'feature_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string * string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string * string) in
    Obj.repr(
# 99 "main.mly"
                                                ( AttributeNoInit(_1, _3) )
# 651 "main.ml"
               : 'feature))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string * string) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string * string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : expr) in
    Obj.repr(
# 100 "main.mly"
                                                ( AttributeInit(_1, _3, _5) )
# 662 "main.ml"
               : 'feature))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : string * string) in
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'formal_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : string * string) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : expr) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 102 "main.mly"
                                                ( Method(_1, _3, _6, _8) )
# 677 "main.ml"
               : 'feature))
; (fun __caml_parser_env ->
    Obj.repr(
# 106 "main.mly"
                                                ( [] )
# 683 "main.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 107 "main.mly"
                                                ( [_1] )
# 690 "main.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 108 "main.mly"
                                                ( _1 :: _3 )
# 699 "main.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string * string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string * string) in
    Obj.repr(
# 112 "main.mly"
                                                ( _1, _3 )
# 708 "main.ml"
               : 'formal))
; (fun __caml_parser_env ->
    Obj.repr(
# 116 "main.mly"
                                                ( [] )
# 714 "main.ml"
               : expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : expr) in
    Obj.repr(
# 117 "main.mly"
                                                ( [_1] )
# 721 "main.ml"
               : expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : expr list) in
    Obj.repr(
# 118 "main.mly"
                                                ( _1 :: _3 )
# 730 "main.ml"
               : expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string * string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : expr) in
    Obj.repr(
# 126 "main.mly"
                                                ( let (l,_) = _1 in (l, Assign(_1, _3)) )
# 739 "main.ml"
               : expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : expr) in
    Obj.repr(
# 127 "main.mly"
                                                ( _1 )
# 746 "main.ml"
               : expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : expr) in
    Obj.repr(
# 131 "main.mly"
                                                ( let (l,_) = _1 in (l, Lt(_1, _3)) )
# 755 "main.ml"
               : expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : expr) in
    Obj.repr(
# 132 "main.mly"
                                                ( let (l,_) = _1 in (l, Le(_1, _3)) )
# 764 "main.ml"
               : expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : expr) in
    Obj.repr(
# 133 "main.mly"
                                                ( let (l,_) = _1 in (l, Equals(_1, _3)) )
# 773 "main.ml"
               : expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : expr) in
    Obj.repr(
# 134 "main.mly"
                                                ( _1 )
# 780 "main.ml"
               : expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : expr) in
    Obj.repr(
# 138 "main.mly"
                                                ( let (l,_) = _1 in (l, Plus(_1, _3)) )
# 789 "main.ml"
               : expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : expr) in
    Obj.repr(
# 139 "main.mly"
                                                ( let (l,_) = _1 in (l, Minus(_1, _3)) )
# 798 "main.ml"
               : expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : expr) in
    Obj.repr(
# 140 "main.mly"
                                                ( _1 )
# 805 "main.ml"
               : expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : expr) in
    Obj.repr(
# 144 "main.mly"
                                                ( let (l,_) = _1 in (l, Times(_1, _3)) )
# 814 "main.ml"
               : expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : expr) in
    Obj.repr(
# 145 "main.mly"
                                                ( let (l,_) = _1 in (l, Divide(_1, _3)) )
# 823 "main.ml"
               : expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : expr) in
    Obj.repr(
# 146 "main.mly"
                                                ( _1 )
# 830 "main.ml"
               : expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : expr) in
    Obj.repr(
# 151 "main.mly"
                                                ( (_1, Tilde(_2)) )
# 838 "main.ml"
               : expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : expr) in
    Obj.repr(
# 152 "main.mly"
                                                ( (_1, Not(_2)) )
# 846 "main.ml"
               : expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : expr) in
    Obj.repr(
# 153 "main.mly"
                                                ( (_1, Isvoid(_2)) )
# 854 "main.ml"
               : expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : expr) in
    Obj.repr(
# 154 "main.mly"
                                                ( _1 )
# 861 "main.ml"
               : expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : call_suffix list) in
    Obj.repr(
# 160 "main.mly"
      (
        let rec fold recv = function
          | [] -> recv
          | CallDyn (m, args) :: tl ->
              let (line, _) = recv in
              fold (line, DynamicDispatch(recv, m, args)) tl
          | CallStat (ty, m, args) :: tl ->
              let (line, _) = recv in
              fold (line, StaticDispatch(recv, ty, m, args)) tl
        in
        fold _1 _2
      )
# 880 "main.ml"
               : expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 5 : expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 176 "main.mly"
                                                ( (_1, If(_2, _4, _6)) )
# 893 "main.ml"
               : expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 177 "main.mly"
                                                ( (_1, While(_2, _4)) )
# 904 "main.ml"
               : expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'let_binding_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : expr) in
    Obj.repr(
# 178 "main.mly"
                                                ( (_1, Let(_2, _4)) )
# 914 "main.ml"
               : expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'case_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 179 "main.mly"
                                                ( (_1, Case(_2, _4)) )
# 925 "main.ml"
               : expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string * string) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : expr list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 182 "main.mly"
                                                ( let (line, _) = _1 in (line, SelfDispatch(_1, _3)) )
# 935 "main.ml"
               : expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string * string) in
    Obj.repr(
# 183 "main.mly"
                                                ( (_1, New(_2)) )
# 943 "main.ml"
               : expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 184 "main.mly"
                                                ( _2 )
# 952 "main.ml"
               : expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : expr list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 185 "main.mly"
                                                ( (_1, Block _2) )
# 961 "main.ml"
               : expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string * string) in
    Obj.repr(
# 186 "main.mly"
                                                ( let (line, _) = _1 in (line, Identifier(_1)) )
# 968 "main.ml"
               : expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string * string) in
    Obj.repr(
# 187 "main.mly"
                                                ( let (line, lit) = _1 in (line, Integer(lit)) )
# 975 "main.ml"
               : expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string * string) in
    Obj.repr(
# 188 "main.mly"
                                                ( let (line, lit) = _1 in (line, String(lit)) )
# 982 "main.ml"
               : expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 189 "main.mly"
                                                ( (_1, True) )
# 989 "main.ml"
               : expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 190 "main.mly"
                                                ( (_1, False) )
# 996 "main.ml"
               : expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 194 "main.mly"
                                                ( [] )
# 1002 "main.ml"
               : call_suffix list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : call_suffix) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : call_suffix list) in
    Obj.repr(
# 195 "main.mly"
                                                ( _1 :: _2 )
# 1010 "main.ml"
               : call_suffix list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string * string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : expr list) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 199 "main.mly"
                                                ( CallDyn (_2, _4) )
# 1021 "main.ml"
               : call_suffix))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string * string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string * string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : expr list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 201 "main.mly"
                                                ( CallStat (_2, _4, _6) )
# 1034 "main.ml"
               : call_suffix))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : expr list) in
    Obj.repr(
# 206 "main.mly"
                                                ( _1 :: _3 )
# 1043 "main.ml"
               : expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 207 "main.mly"
                                                ( [_1] )
# 1051 "main.ml"
               : expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string * string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string * string) in
    Obj.repr(
# 211 "main.mly"
                                                ( LetBindingNoInit(_1, _3) )
# 1060 "main.ml"
               : 'let_binding))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string * string) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string * string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : expr) in
    Obj.repr(
# 212 "main.mly"
                                                ( LetBindingInit(_1, _3, _5) )
# 1071 "main.ml"
               : 'let_binding))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'let_binding) in
    Obj.repr(
# 216 "main.mly"
                                                ( [_1] )
# 1078 "main.ml"
               : 'let_binding_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'let_binding) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'let_binding_list) in
    Obj.repr(
# 217 "main.mly"
                                                ( _1 :: _3 )
# 1087 "main.ml"
               : 'let_binding_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string * string) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string * string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : expr) in
    Obj.repr(
# 221 "main.mly"
                                                ( (_1, _3, _5) )
# 1098 "main.ml"
               : 'case_branch))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'case_branch) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 225 "main.mly"
                                                ( [_1] )
# 1106 "main.ml"
               : 'case_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'case_branch) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'case_list) in
    Obj.repr(
# 226 "main.mly"
                                                ( _1 :: _3 )
# 1115 "main.ml"
               : 'case_list))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : program)
;;
# 230 "main.mly"

let deserialize infile = 
  let in_channel = open_in infile in
  let queue = Queue.create () in
  let get_line () = input_line in_channel in
  try 
    while true do
      let l = get_line () in
      let token_type = get_line () in
      let token =
        match token_type with
        | "plus"        -> PLUS(l)
        | "minus"       -> MINUS(l)
        | "times"       -> TIMES(l)
        | "divide"      -> DIVIDE(l)
        | "at"          -> AT(l)
        | "lt"          -> LT(l)
        | "le"          -> LE(l)
        | "semi"        -> SEMI(l)
        | "tilde"       -> TILDE(l)
        | "dot"         -> DOT(l)
        | "comma"       -> COMMA(l)
        | "colon"       -> COLON(l)
        | "equals"      -> EQUALS(l)
        | "rbrace"      -> RBRACE(l)
        | "lbrace"      -> LBRACE(l)
        | "rparen"      -> RPAREN(l)
        | "lparen"      -> LPAREN(l)
        | "larrow"      -> LARROW(l)
        | "rarrow"      -> RARROW(l)
        | "true"        -> TRUE(l)
        | "false"       -> FALSE(l)
        | "new"         -> NEW(l)
        | "not"         -> NOT(l)
        | "let"         -> LET(l)
        | "else"        -> ELSE(l)
        | "class"       -> CLASS(l)
        | "case"        -> CASE(l)
        | "esac"        -> ESAC(l)
        | "inherits"    -> INHERITS(l)
        | "loop"        -> LOOP(l)
        | "pool"        -> POOL(l)
        | "isvoid"      -> ISVOID(l)
        | "of"          -> OF(l)
        | "in"          -> IN(l)
        | "if"          -> IF(l)
        | "fi"          -> FI(l)
        | "while"       -> WHILE(l)
        | "then"        -> THEN(l)
        | "integer"     -> INTEGER(l, get_line ())
        | "string"      -> STRING(l, get_line ())
        | "identifier"  -> IDENTIFIER(l, get_line ())
        | "type"        -> TYPE(l, get_line ())
        | _             -> printf "Unexpected token type: %s\n" token_type ; exit 1
      in
      Queue.add (l, token) queue
    done
  with _ ->  ();
  close_in in_channel; 
  queue

let main () = 
begin  
  let infile = Sys.argv.(1) in
  let queue = deserialize infile in
  let lexbuf = Lexing.from_string "" in 
  let last_line_number = ref "1" in
  let token lb = 
    if Queue.is_empty queue then
      EOF
    else 
      let line_number, next_token = Queue.take queue in
      last_line_number := line_number ; 
      next_token
  in 

  let ast =
    try
      program token lexbuf
    with
    | Parsing.Parse_error ->
        let tok =
          if Queue.is_empty queue then "<EOF>"
          else
            let _, t = Queue.peek queue in
            match t with
            | IDENTIFIER (_, n) -> "IDENTIFIER(" ^ n ^ ")"
            | TYPE       (_, n) -> "TYPE(" ^ n ^ ")"
            | STRING     (_, s) -> "STRING(" ^ s ^ ")"
            | INTEGER    (_, i) -> "INTEGER(" ^ i ^ ")"
            | PLUS _            -> "PLUS"
            | MINUS _           -> "MINUS"
            | TIMES _           -> "TIMES"
            | DIVIDE _          -> "DIVIDE"
            | EQUALS _          -> "EQUALS"
            | AT _              -> "AT"
            | LT _              -> "LT"
            | LE _              -> "LE"
            | SEMI _            -> "SEMI"
            | TILDE _           -> "TILDE"
            | DOT _             -> "DOT"
            | COMMA _           -> "COMMA"
            | COLON _           -> "COLON"
            | RBRACE _          -> "RBRACE"
            | LBRACE _          -> "LBRACE"
            | RPAREN _          -> "RPAREN"
            | LPAREN _          -> "LPAREN"
            | LARROW _          -> "LARROW"
            | RARROW _          -> "RARROW"
            | TRUE _            -> "TRUE"
            | FALSE _           -> "FALSE"
            | NEW _             -> "NEW"
            | NOT _             -> "NOT"
            | LET _             -> "LET"
            | ELSE _            -> "ELSE"
            | CLASS _           -> "CLASS"
            | CASE _            -> "CASE"
            | ESAC _            -> "ESAC"
            | INHERITS _        -> "INHERITS"
            | LOOP _            -> "LOOP"
            | POOL _            -> "POOL"
            | ISVOID _          -> "ISVOID"
            | OF _              -> "OF"
            | IN _              -> "IN"
            | IF _              -> "IF"
            | FI _              -> "FI"
            | WHILE _           -> "WHILE"
            | THEN _            -> "THEN"
            | EOF               -> "<EOF>"
        in
        Printf.printf "ERROR: %s: Parser: unexpected %s\n" !last_line_number tok;
        exit 1
    | e ->
        Printf.printf "ERROR: %s: Parser: %s\n"
          !last_line_number (Printexc.to_string e);
        exit 1
  in
  
  let outfile = (Filename.chop_extension Sys.argv.(1)) ^ ".cl-ast" in
  let f = open_out outfile in 

  let rec serialize f ast = 
    serialize_class_list ast
  and serialize_class_list ast = 
    fprintf f "%d\n" (List.length ast) ;
    List.iter serialize_class ast
  and serialize_class ast = 
    match ast with
    | ClassNoInherits(class_name, class_features) -> 
        serialize_identifier class_name ; 
        fprintf f "no_inherits\n" ; 
        fprintf f "%d\n" (List.length class_features) ; 
        List.iter serialize_feature class_features
    | ClassInherits(class_name, parent_name, class_features) ->
        serialize_identifier class_name ; 
        fprintf f "inherits\n" ; 
        serialize_identifier parent_name ;
        fprintf f "%d\n" (List.length class_features) ; 
        List.iter serialize_feature class_features
  and serialize_identifier (line, lexeme) =
    fprintf f "%s\n%s\n" line lexeme
  and serialize_feature ast = 
    match ast with
    | AttributeNoInit(attr_name, attr_type) -> 
        fprintf f "attribute_no_init\n" ; 
        serialize_identifier attr_name ; 
        serialize_identifier attr_type
    | AttributeInit(attr_name, attr_type, init_expr) ->
        fprintf f "attribute_init\n" ; 
        serialize_identifier attr_name ; 
        serialize_identifier attr_type ;
        serialize_expr init_expr
    | Method(method_name, method_formals, method_type, method_body) ->
        fprintf f "method\n" ;
        serialize_identifier method_name ; 
        fprintf f "%d\n" (List.length method_formals);  
        List.iter serialize_formal method_formals ; 
        serialize_identifier method_type ;
        serialize_expr method_body
  and serialize_formal (formal_name, formal_type) = 
    serialize_identifier formal_name ;
    serialize_identifier formal_type
  and serialize_expr (line, expr_internal) = 
    fprintf f "%s\n" line ; 
    match expr_internal with
    | Assign(lh_value, rh_value) ->
        fprintf f "assign\n" ; 
        serialize_identifier lh_value ;
        serialize_expr rh_value
    | DynamicDispatch(e, method_name, args) -> 
        fprintf f "dynamic_dispatch\n" ; 
        serialize_expr e ; 
        serialize_identifier method_name ;
        fprintf f "%d\n" (List.length args) ; 
        List.iter serialize_expr args
    | StaticDispatch(e, type_name, method_name, args) -> 
        fprintf f "static_dispatch\n" ;
        serialize_expr e ; 
        serialize_identifier type_name ;  
        serialize_identifier method_name ;
        fprintf f "%d\n" (List.length args) ; 
        List.iter serialize_expr args
    | SelfDispatch(method_name, args) -> 
        fprintf f "self_dispatch\n" ; 
        serialize_identifier method_name ; 
        fprintf f "%d\n" (List.length args) ; 
        List.iter serialize_expr args
    | If(p, t, e2) -> 
        fprintf f "if\n" ; 
        serialize_expr p ; 
        serialize_expr t ;
        serialize_expr e2
    | While(p, b) -> 
        fprintf f "while\n" ; 
        serialize_expr p ; 
        serialize_expr b 
    | Let(bindings, body_expr) ->
        fprintf f "let\n" ;
        fprintf f "%d\n" (List.length bindings) ;
        List.iter (function
          | LetBindingNoInit (v, ty) ->
              fprintf f "let_binding_no_init\n" ;
              serialize_identifier v ;
              serialize_identifier ty
          | LetBindingInit (v, ty, init_e) ->
              fprintf f "let_binding_init\n" ;
              serialize_identifier v ;
              serialize_identifier ty ;
              serialize_expr init_e
        ) bindings ;
        serialize_expr body_expr
    | Case(scrutinee, branches) -> 
        fprintf f "case\n" ; 
        serialize_expr scrutinee ; 
        fprintf f "%d\n" (List.length branches) ; 
        List.iter (fun (id1, id2, e) -> 
          serialize_identifier id1 ; 
          serialize_identifier id2 ; 
          serialize_expr e
        ) branches
    | New(obj_type) ->
        fprintf f "new\n" ; 
        serialize_identifier obj_type
    | Isvoid(e1) -> 
        fprintf f "isvoid\n" ; 
        serialize_expr e1
    | Plus(x, y) ->
        fprintf f "plus\n" ;  serialize_expr x ; serialize_expr y
    | Minus(x, y) ->
        fprintf f "minus\n" ; serialize_expr x ; serialize_expr y
    | Times(x, y) -> 
        fprintf f "times\n" ; serialize_expr x ; serialize_expr y
    | Divide(x, y) -> 
        fprintf f "divide\n" ; serialize_expr x ; serialize_expr y
    | Tilde(e1) -> 
        fprintf f "negate\n" ; serialize_expr e1
    | Lt(x, y) ->
        fprintf f "lt\n" ; serialize_expr x ; serialize_expr y
    | Le(x, y) ->
        fprintf f "le\n" ; serialize_expr x ; serialize_expr y
    | Equals(x, y) ->
        fprintf f "eq\n" ; serialize_expr x ; serialize_expr y
    | Not(e1) -> 
        fprintf f "not\n" ; serialize_expr e1
    | Identifier id -> 
        fprintf f "identifier\n"; serialize_identifier id
    | Integer i ->
        fprintf f "integer\n%s\n" i
    | String s -> 
        fprintf f "string\n%s\n" s
    | True -> 
        fprintf f "true\n" 
    | False -> 
        fprintf f "false\n" 
    | Block es ->
        fprintf f "block\n";
        fprintf f "%d\n" (List.length es);
        List.iter serialize_expr es
  in  

  serialize f ast; 
  close_out f;
end;;
main ();;
# 1425 "main.ml"
