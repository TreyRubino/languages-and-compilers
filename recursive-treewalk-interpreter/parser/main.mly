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
and expr = string * expr_internal

type program = structure list

%}

%token <string * string> STRING IDENTIFIER TYPE INTEGER
%token <string> PLUS MINUS TIMES DIVIDE EQUALS
%token <string> AT LT SEMI TILDE DOT COMMA COLON
%token <string> RBRACE LBRACE RPAREN LPAREN LE
%token <string> LARROW RARROW TRUE FALSE NEW NOT
%token <string> LET ELSE CLASS CASE INHERITS LOOP
%token <string> POOL ISVOID OF IN IF FI WHILE THEN
%token EOF

%right LARROW
%left NOT
%nonassoc LE LT EQUALS
%left PLUS MINUS
%left TIMES DIVIDE
%left ISVOID
%left TILDE
%left AT
%left DOT

%start program
%type <program> program

%% 

program: 
| class_list                                                                  { $1 }
;

class_list: 
| /* lambda */                                                                { [] }
| structure SEMI class_list                                                   { $1 :: $3 }
;

structure:
| CLASS TYPE LBRACE feature_list RBRACE                                        { ClassNoInherits($2, $4) } 
| CLASS TYPE INHERITS TYPE LBRACE feature_list RBRACE                          { ClassInherits($2, $4, $6) }
;    

feature_list:
| /* lambda */                                                                { [] }
| feature SEMI feature_list                                                   { $1 :: $3 }
;

feature:
| IDENTIFIER COLON TYPE                                                       { AttributeNoInit($1, $3) }  
| IDENTIFIER COLON TYPE LARROW expr                                           { AttributeInit($1, $3, $5) }
| IDENTIFIER LPAREN formal_list RPAREN COLON TYPE LBRACE expr RBRACE          { Method($1, $3, $6, $8) }  
; 

formal_list:
| /* lambda */                                                                { [] }
| formal COMMA formal_list                                                    { $1 :: $3 } 
;

formal:
| IDENTIFIER COLON TYPE                                                       { $1, $3 }
;

expr_list :
| /* lambda */                                                                { [] }
| expr COMMA expr_list                                                        { $1 :: $3 }

expr :
| IDENTIFIER LARROW expr                                                      { let line, _ = $1 in (line, Assign($1, $3)) }
| expr AT TYPE DOT IDENTIFIER LPAREN expr_list RPAREN                         { let line, _ = $1 in (line, StaticDispatch($1, $3, $5, $7)) }
| expr DOT IDENTIFIER LPAREN expr_list RPAREN                                 { let line, _ = $1 in (line, DynamicDispatch($1, $3, $5)) }
| IDENTIFIER LPAREN expr_list RPAREN                                          { let line, _ = $1 in (line, SelfDispatch($1, $3)) }
| IF expr THEN expr ELSE expr FI                                              { ($1, If($2, $4, $6)) }
| WHILE expr LOOP expr POOL                                                   { ($1, While($2, $4)) }
| NEW TYPE                                                                    { ($1, New($2)) }
| ISVOID expr                                                                 { ($1, Isvoid($2)) }
| expr PLUS expr                                                              { let line, _ = $1 in (line, Plus($1, $3)) }
| expr MINUS expr                                                             { let line, _ = $1 in (line, Minus($1, $3)) }
| expr TIMES expr                                                             { let line, _ = $1 in (line, Times($1, $3)) }
| expr DIVIDE expr                                                            { let line, _ = $1 in (line, Divide($1, $3)) }
| TILDE expr                                                                  { ($1, Tilde($2)) }
| expr LT expr                                                                { let line, _ = $1 in (line, Lt($1, $3)) }
| expr LE expr                                                                { let line, _ = $1 in (line, Le($1, $3)) }
| expr EQUALS expr                                                            { let line, _ = $1 in (line, Equals($1, $3)) }
| NOT expr                                                                    { ($1, Not($2)) }
| IDENTIFIER                                                                  { let line, id = $1 in (line, Identifier(line, id)) }
| INTEGER                                                                     { let line, int = $1 in (line, Integer(int)) }
| STRING                                                                      { let line, string = $1 in (line, String(string)) }
| TRUE                                                                        { ($1, True) }
| FALSE                                                                       { ($1, False) }
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
            let token = match token_type with
            | "plus"        -> PLUS(l)
            | "minus"       -> MINUS(l)
            | "times"        -> TIMES(l)
            | "divide"      -> DIVIDE(l)
            | "at"          -> AT(l)
            | "lt"          -> LT(l)
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
            | "true"        -> TRUE(l)
            | "false"       -> FALSE(l)
            | "new"         -> NEW(l)
            | "not"         -> NOT(l)
            | "let"         -> LET(l)
            | "else"        -> ELSE(l)
            | "class"       -> CLASS(l)
            | "case"        -> CASE(l)
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
    with _ ->  () ;          
    close_in in_channel ; 
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
        with _ -> printf "ERROR: %s: Parser: message\n" !last_line_number ; exit 1
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
        | If(predicate, then_br, else_br) -> 
            serialize_expr predicate ; 
            serialize_expr then_br ;
            serialize_expr else_br
        | While(predicate, while_body) -> 
            fprintf f "while\n" ; 
            serialize_expr predicate ; 
            serialize_expr while_body 
        | New(obj_type) ->
            fprintf f "new\n" ; 
            serialize_identifier obj_type
        | Isvoid(sub1) -> 
            fprintf f "isvoid\n" ; 
            serialize_expr sub1
        | Plus(sub1, sub2) ->
            fprintf f "plus\n" ;
            serialize_expr sub1 ; 
            serialize_expr sub2
        | Minus(sub1, sub2) ->
            fprintf f "minus\n" ;
            serialize_expr sub1 ; 
            serialize_expr sub2  
        | Times(sub1, sub2) -> 
            fprintf f "times\n" ;
            serialize_expr sub1 ; 
            serialize_expr sub2
        | Divide(sub1, sub2) -> 
            fprintf f "divide\n" ;
            serialize_expr sub1 ; 
            serialize_expr sub2
        | Tilde(sub1) -> 
            fprintf f "negate\n" ; 
            serialize_expr sub1
        | Lt(sub1, sub2) ->
            fprintf f "lt\n" ;
            serialize_expr sub1 ;
            serialize_expr sub2
        | Le(sub1, sub2) ->
            fprintf f "le\n" ;
            serialize_expr sub1 ; 
            serialize_expr sub2
        | Equals(sub1, sub2) ->
            fprintf f "eq\n" ;
            serialize_expr sub1 ;
            serialize_expr sub2
        | Not(sub1) -> 
            fprintf f "not\n" ; 
            serialize_expr sub1
        | Identifier(id) -> 
            fprintf f "identifier\n";
            serialize_identifier id
        | Integer(int) ->
            fprintf f "integer\n%s\n" int
        | String(string) -> 
            fprintf f "string\n%s\n" string
        | True -> 
            fprintf f "true\n" 
        | False -> 
            fprintf f "false\n" 
    in  

    serialize f ast; 
    close_out f;
end ;;
main () ;;