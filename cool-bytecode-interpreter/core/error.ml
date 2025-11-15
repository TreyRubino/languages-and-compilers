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

let checker loc fmt = 
  raisef ~phase:"Type-Check" ~line:loc fmt

let codegen loc fmt =
  raisef ~phase:"Codegen" ~line:loc fmt

let vm pc fmt = 
  raisef ~phase:"Exception" ~line:pc fmt
