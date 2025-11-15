(*
@author Trey Rubino
@date 11/13/2025
*)

let read_file filename = 
  let in_channel = open_in filename in
  let len = in_channel_length in_channel in
  let s = really_input_string in_channel len in
  close_in in_channel;
  s

let () = 
  if Array.length Sys.argv < 2 then (
    Printf.eprintf "usage: %s <file>\n" Sys.argv.(0);
    exit 1
  );

  let filename = Sys.argv.(1) in
  let source = read_file filename in

  let lexbuf = Lexing.from_string source in
  let ast =
    try Parser.cool_program Lexer.token lexbuf
    with 
    | Error.E e -> Error.print e; exit 1
    | Parsing.Parse_error -> Error.parser lexbuf; exit 1
  in

  (* checker *)
  let typed = 
    try Checker.check ast
    with
    | Error.E e -> Error.print e; exit 1
  in

  (* codegen *)

  (* vm *)
  ()