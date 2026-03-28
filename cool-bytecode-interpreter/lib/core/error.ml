(*
@author Trey Rubino
@date 11/15/2025
*)

type t = {
  phase : string;
  line  : string;
  msg   : string
}

exception E of t

let raisef ~phase ~line fmt =
  Printf.ksprintf (fun msg ->
    raise (E { phase; line; msg })
  ) fmt

let print { phase; line; msg } = 
  Printf.printf "ERROR: %s: %s: %s\n" line phase msg

let lexer lexbuf fmt = 
  let line = string_of_int lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum in
  raisef ~phase:"Lexer" ~line fmt

let parser lexbuf = 
  let line = string_of_int lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum in
  let lexeme = Lexing.lexeme lexbuf in
  print {
    phase = "Parser";
    line;
    msg = Printf.sprintf "syntax error near %s" lexeme;
  }

let checker loc fmt =
  raisef ~phase:"Type-Check" ~line:loc fmt

let codegen loc fmt =
  raisef ~phase:"Codegen" ~line:loc fmt

let vm pc fmt = 
  raisef ~phase:"Exception" ~line:pc fmt

  