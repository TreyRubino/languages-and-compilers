(*
@author Trey Rubino
@date 11/15/2025
*)

type t = {
  phase : string;
  line  : int;
  msg   : string
}

exception E of t

let raisef ~phase ~line fmt =
  Printf.ksprintf (fun msg ->
    raise (E { phase; line; msg })
  ) fmt

let print { phase; line; msg } = 
  Printf.printf "ERROR: %d: %s: %s\n" line phase msg

let lexer lexbuf fmt = 
  let line = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum in
  raisef ~phase:"Lexer" ~line fmt

let parser lexbuf = 
  let line = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum in
  let lexeme = Lexing.lexeme lexbuf in
  print {
    phase = "Parser";
    line;
    msg = Printf.sprintf "syntax error near %s" lexeme;
  }

let checker loc fmt =
  let line =
    try int_of_string loc
    with _ -> 0
  in
  raisef ~phase:"Type-Check" ~line fmt

let codegen loc fmt =
  raisef ~phase:"Codegen" ~line:loc fmt

let vm pc fmt = 
  raisef ~phase:"Exception" ~line:pc fmt

  