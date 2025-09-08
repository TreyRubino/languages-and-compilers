%{
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
%type <expr> expr cmp_expr sum_expr product_expr unary_expr primary_expr primary_base
%type <call_suffix list> primary_suffixes
%type <call_suffix> primary_suffix
%type <expr list> expr_list block_elems

%start program

%%

program:
    class_list                                  { $1 }
  ;

class_list:
    /* lambda */                                { [] }
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
    IDENTIFIER LARROW expr                      { let (l,_) = $1 in (l, Assign($1, $3)) }
  | IF expr THEN expr ELSE expr FI              { ($1, If($2, $4, $6)) }
  | WHILE expr LOOP expr POOL                   { ($1, While($2, $4)) }
  | LET let_binding_list IN expr                { ($1, Let($2, $4)) }
  | CASE expr OF case_list ESAC                 { ($1, Case($2, $4)) }
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
  | primary_expr                                { $1 }
  ;

primary_expr:
    primary_base primary_suffixes
      {
        let rec fold recv = function
          | [] -> recv
          | CallDyn (m, args) :: tl ->
              let (line, _) = recv in
              fold (line, DynamicDispatch(recv, m, args)) tl
          | CallStat (ty, m, args) :: tl ->
              let (line, _) = recv in
              fold (line, StaticDispatch(recv, ty, m, args)) tl
        in
        fold $1 $2
      }
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

primary_suffixes:
    /* lambda */                                { [] }
  | primary_suffix primary_suffixes             { $1 :: $2 }
  ;

primary_suffix:
    DOT IDENTIFIER LPAREN expr_list RPAREN      { CallDyn ($2, $4) }
  | AT TYPE DOT IDENTIFIER LPAREN expr_list RPAREN
                                                { CallStat ($2, $4, $6) }
  ;

block_elems:
    expr SEMI block_elems                       { $1 :: $3 }
  | expr SEMI                                   { [$1] }
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

let unserialize infile = 
  let in_channel = open_in infile in
  let queue = Queue.create () in
  let get_line () = String.trim (input_line in_channel) in
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
  let queue = unserialize infile in
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