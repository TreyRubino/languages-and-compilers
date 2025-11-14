(*
@author Trey Rubino
@date 11/13/2025
*)

%{
open Ast
exception Error of string
%}

%token <string * string> STRING IDENTIFIER TYPE INTEGER
%token <string> PLUS MINUS TIMES DIVIDE EQUALS
%token <string> AT LT SEMI TILDE DOT COMMA COLON
%token <string> RBRACE LBRACE RPAREN LPAREN LE
%token <string> LARROW RARROW TRUE FALSE NEW NOT
%token <string> LET ELSE CLASS CASE ESAC INHERITS LOOP
%token <string> POOL ISVOID OF IN IF FI WHILE THEN
%token EOF

%right LARROW
%left NOT
%left LT LE EQUALS
%left PLUS MINUS
%left TIMES DIVIDE
%left ISVOID
%left TILDE
%left AT
%left DOT

%type <program> program
%type <expr> expr assign_expr cmp_expr sum_expr product_expr unary_expr primary_expr primary_base atom
%type <expr list> expr_list block_elems

%start program

%%

program:
    class_list                                  { $1 }
  ;

class_list:
    structure SEMI                              { [$1] }
  | structure SEMI class_list                   { $1 :: $3 }
  ;

structure:
    CLASS TYPE LBRACE feature_list RBRACE                 { ClassNoInherits($2, $4) } 
  | CLASS TYPE INHERITS TYPE LBRACE feature_list RBRACE   { ClassInherits($2, $4, $6) }
  ;

feature_list:
    /* lambda */                                { [] }
  | feature SEMI feature_list                   { $1 :: $3 }
  ;

feature:
    IDENTIFIER COLON TYPE                       { AttributeNoInit($1, $3) }  
  | IDENTIFIER COLON TYPE LARROW expr           { AttributeInit($1, $3, $5) }
  | IDENTIFIER LPAREN formal_list RPAREN COLON TYPE LBRACE expr RBRACE
                                                { Method($1, $3, $6, $8) }  
  ;

formal_list:
    /* lambda */                                { [] }
  | formal                                      { [$1] }
  | formal COMMA formal_list                    { $1 :: $3 } 
  ;

formal:
    IDENTIFIER COLON TYPE                       { $1, $3 }
  ;

expr_list:
    /* lambda */                                { [] }
  | expr                                        { [$1] }
  | expr COMMA expr_list                        { $1 :: $3 }
  ;

expr:
  |  assign_expr                                { $1 }
  ;

assign_expr:
    IDENTIFIER LARROW expr                      { let (l,_) = $1 in (l, Assign($1, $3)) }
  | cmp_expr                                    { $1 }
  ;

cmp_expr:
    cmp_expr LT     sum_expr                    { let (l,_) = $1 in (l, Lt($1, $3)) }
  | cmp_expr LE     sum_expr                    { let (l,_) = $1 in (l, Le($1, $3)) }
  | cmp_expr EQUALS sum_expr                    { let (l,_) = $1 in (l, Equals($1, $3)) }
  | sum_expr                                    { $1 }
  ;

sum_expr:
    sum_expr PLUS  product_expr                 { let (l,_) = $1 in (l, Plus($1, $3)) }
  | sum_expr MINUS product_expr                 { let (l,_) = $1 in (l, Minus($1, $3)) }
  | product_expr                                { $1 }
  ;

product_expr:
    product_expr TIMES  unary_expr              { let (l,_) = $1 in (l, Times($1, $3)) }
  | product_expr DIVIDE unary_expr              { let (l,_) = $1 in (l, Divide($1, $3)) }
  | unary_expr                                  { $1 }
  ;

unary_expr:
    TILDE  unary_expr                           { ($1, Tilde($2)) }
  | NOT    unary_expr                           { ($1, Not($2)) }
  | ISVOID unary_expr                           { ($1, Isvoid($2)) }
  | atom                                        { $1 }
  ;

atom: 
    IF expr THEN expr ELSE expr FI              { ($1, If($2, $4, $6)) }
  | WHILE expr LOOP expr POOL                   { ($1, While($2, $4)) }
  | LET let_binding_list IN expr                { ($1, Let($2, $4)) }
  | CASE expr OF case_list ESAC                 { ($1, Case($2, $4)) }
  | primary_expr                                { $1 }
  ; 

primary_expr:
    primary_base                                { $1 }
  | primary_expr DOT IDENTIFIER LPAREN expr_list RPAREN
                                                { let (line, _) = $1 in (line, DynamicDispatch($1, $3, $5)) }
  | primary_expr AT TYPE DOT IDENTIFIER LPAREN expr_list RPAREN
                                                { let (line, _) = $1 in (line, StaticDispatch($1, $3, $5, $7)) }
  ;

primary_base:
    IDENTIFIER LPAREN expr_list RPAREN          { let (line, _) = $1 in (line, SelfDispatch($1, $3)) }
  | NEW TYPE                                    { ($1, New($2)) }
  | LPAREN expr RPAREN                          { $2 }
  | LBRACE block_elems RBRACE                   { ($1, Block $2) }
  | IDENTIFIER                                  { let (line, _) = $1 in (line, Identifier($1)) }
  | INTEGER                                     { let (line, lit) = $1 in (line, Integer(lit)) }
  | STRING                                      { let (line, lit) = $1 in (line, String(lit)) }
  | TRUE                                        { ($1, True) }
  | FALSE                                       { ($1, False) }
  ;

block_elems:
    expr SEMI                                   { [$1] }
  | expr SEMI block_elems                       { $1 :: $3 }
  ;

let_binding:
    IDENTIFIER COLON TYPE                       { LetBindingNoInit($1, $3) }
  | IDENTIFIER COLON TYPE LARROW expr           { LetBindingInit($1, $3, $5) }
  ;

let_binding_list:
    let_binding                                 { [$1] }
  | let_binding COMMA let_binding_list          { $1 :: $3 }
  ;

case_branch:
    IDENTIFIER COLON TYPE RARROW expr           { ($1, $3, $5) }
  ;

case_list:
    case_branch SEMI                            { [$1] }
  | case_branch SEMI case_list                  { $1 :: $3 }
  ;

%%

let lexeme_of_token = function
  | IDENTIFIER (_, n) -> n
  | TYPE       (_, n) -> n
  | STRING     (_, s) -> "\"" ^ s ^ "\""
  | INTEGER    (_, i) -> i
  | PLUS _            -> "+"
  | MINUS _           -> "-"
  | TIMES _           -> "*"
  | DIVIDE _          -> "/"
  | EQUALS _          -> "="
  | AT _              -> "@"
  | LT _              -> "<"
  | LE _              -> "<="
  | SEMI _            -> ";"
  | TILDE _           -> "~"
  | DOT _             -> "."
  | COMMA _           -> ","
  | COLON _           -> ":"
  | RBRACE _          -> "}"
  | LBRACE _          -> "{"
  | RPAREN _          -> ")"
  | LPAREN _          -> "("
  | LARROW _          -> "<-"
  | RARROW _          -> "=>"
  | TRUE _            -> "true"
  | FALSE _           -> "false"
  | NEW _             -> "new"
  | NOT _             -> "not"
  | LET _             -> "let"
  | ELSE _            -> "else"
  | CLASS _           -> "class"
  | CASE _            -> "case"
  | ESAC _            -> "esac"
  | INHERITS _        -> "inherits"
  | LOOP _            -> "loop"
  | POOL _            -> "pool"
  | ISVOID _          -> "isvoid"
  | OF _              -> "of"
  | IN _              -> "in"
  | IF _              -> "if"
  | FI _              -> "fi"
  | WHILE _           -> "while"
  | THEN _            -> "then"
  | EOF               -> "EOF"

let line_of_token = function
  | STRING (l, _) 
  | IDENTIFIER (l, _)
  | TYPE (l, _)
  | INTEGER (l, _) -> l
  | PLUS l | MINUS l | TIMES l | DIVIDE l | EQUALS l
  | AT l | LT l | LE l | SEMI l | TILDE l | DOT l
  | COMMA l | COLON l | RBRACE l | LBRACE l | RPAREN l | LPAREN l
  | LARROW l | RARROW l
  | TRUE l | FALSE l | NEW l | NOT l
  | LET l | ELSE l | CLASS l | CASE l | ESAC l | INHERITS l
  | LOOP l | POOL l | ISVOID l | OF l | IN l
  | IF l | FI l | WHILE l | THEN l -> l
  | EOF -> "1"

let parse (tokens : Tokens.token list) : program = 
  let q = Queue.create () in 
  List.iter (fun t -> Queue.add t q) tokens;

  let last_token : Tokens.token option ref = ref None in
  let next_token _ =
    if Queue.is_empty q then (
      last_token := Some EOF;
      EOF
    ) else (
      let t = Queue.pop q in 
      last_token := Some t;
      t
    )
  in

  let lexbuf = Lexing.from_string "" in
  try 
    program next_token lexbuf
  with
  | _ -> 
    let line, near = 
      match !last_token with
      | Some t -> line_of_token t, lexeme_of_token t
      | None -> 
      if Queue.is_empty q then 
        ("1", "EOF")
      else 
        let t = Queue.peek q in 
        (line_of_token t, lexeme_of_token t)
    in
    let msg = Printf.sprintf "ERROR: %s: Parser: syntax error near %s" line near in
    raise (Error msg)


