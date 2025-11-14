(* 
@author Trey Rubino
@date 11/13/2025
*)

{
open Core.Tokens
exception Error of string

let lex_error lexbuf fmt = 
  let line = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum in
  let lex = Lexing.lexeme lexbuf in
  let msg = Printf.sprintf fmt line lex in
  raise (Error msg)
}

rule token = parse
  | [' ' '\t' '\r' '\012' '\011']	                                            { token lexbuf } 
  | '\n'		                                                                  { Lexing.new_line lexbuf ; token lexbuf  }

  | "--" [^ '\n']*                                                            { token lexbuf }
  | "(*"                                                                      { comment 1 lexbuf.lex_curr_p.pos_lnum lexbuf }
          
  | '+'			                                                                  { PLUS } 
  | '-'			                                                                  { MINUS }
  | '*' 		                                                                  { TIMES }
  | '/' 		                                                                  { DIVIDE }
  | '='                                                                       { EQUALS }
  | '@'                                                                       { AT }
  | '<'                                                                       { LT }
  | ';'                                                                       { SEMI }
  | '~'                                                                       { TILDE }
  | '.'                                                                       { DOT }
  | ','                                                                       { COMMA }
  | ':'                                                                       { COLON }
  | '}'                                                                       { RBRACE }
  | '{'                                                                       { LBRACE }
  | ')'                                                                       { RPAREN }
  | '('                                                                       { LPAREN }
  | "<="                                                                      { LE }   
  | "<-"                                                                      { LARROW }
  | "=>"                                                                      { RARROW }

  | 't'['r' 'R']['u' 'U']['e' 'E']                                            { TRUE } 
  | 'f'['a' 'A']['l' 'L']['s' 'S']['e' 'E']                                   { FALSE }

  | ['n' 'N']['e' 'E']['w' 'W']                                               { NEW }
  | ['n' 'N']['o' 'O']['t' 'T']                                               { NOT }
  | ['l' 'L']['e' 'E']['t' 'T']                                               { LET }
  | ['e' 'E']['l' 'L']['s' 'S']['e' 'E']                                      { ELSE }
  | ['c' 'C']['l' 'L']['a' 'A']['s' 'S']['s' 'S']                             { CLASS }
  | ['c' 'C']['a' 'A']['s' 'S']['e' 'E']                                      { CASE }
  | ['e' 'E']['s' 'S']['a' 'A']['c' 'C']                                      { ESAC }
  | ['i' 'I']['n' 'N']['h' 'H']['e' 'E']['r' 'R']['i' 'I']['t' 'T']['s' 'S']  { INHERITS }
  | ['l' 'L']['o' 'O']['o' 'O']['p' 'P']                                      { LOOP }
  | ['p' 'P']['o' 'O']['o' 'O']['l' 'L']                                      { POOL }
  | ['i' 'I']['s' 'S']['v' 'V']['o' 'O']['i' 'I']['d' 'D']                    { ISVOID }
  | ['o' 'O']['f' 'F']                                                        { OF }
  | ['i' 'I']['n' 'N']                                                        { IN }
  | ['i' 'I']['f' 'F']                                                        { IF }
  | ['f' 'F']['i' 'I']                                                        { FI }
  | ['w' 'W']['h' 'H']['i' 'I']['l' 'L']['e' 'E']                             { WHILE }
  | ['t' 'T']['h' 'H']['e' 'E']['n' 'N']                                      { THEN }

  | ['0'-'9']+ as lxm 	                                                      { INTEGER(int_of_string lxm) }
  | ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm                            { TYPE(lxm) }
  | ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm                            { IDENTIFIER(lxm) }    

  | '"' ([^ '"' '\\' '\n' '\000'] | '\\' _)* '\000' [^ '"']* '"'              { lex_error lexbuf "ERROR: %d: Lexer: null in string: %s" }
  | '"' ( [^ '"' '\\' '\n'] | '\\' _ )* '"' as lxm  { if String.length lxm > 1024 then ( lex_error lexbuf "ERROR: %d: Lexer: string constant is too long: %s" ) else STRING(String.sub lxm 1 (String.length lxm - 2)) }

  | _                                                                         { lex_error lexbuf "ERROR: %d: Lexer: invalid character: %s" }
  | eof 			                                                                { raise End_of_file }

and comment depth start_line = parse
  | "(*"                                                                      { comment (depth + 1) start_line lexbuf }
  | "*)"                                                                      { if depth = 1 then token lexbuf else comment (depth - 1) start_line lexbuf }
  | '\n'                                                                      { Lexing.new_line lexbuf ; comment depth start_line lexbuf }
  | eof                                                                       { lex_error lexbuf "ERROR: %d: Lexer: EOF in comment: %s" }
  | _                                                                         { comment depth start_line lexbuf }

{
let tokenize (source : string) : Tokens.token list = 
  let lexbuf = Lexing.from_string source in 
  let rec loop acc = 
    try
      let tok = token lexbuf in
      loop (tok :: acc)
    with
    End_of_file -> List.rev acc
  in
  loop []
}