(*
@author Trey Rubino
@date 11/13/2025
*)

open Frontend

let read_file filename = 
  let in_channel = open_in filename in
  let len = in_channel_length in_channel in
  let s = input_string in_channel len in
  close_in in_channel;
  s

let () = 
  if Array.length Sys.argv < then (
    Printf.eprintf "usage: %s <file>\n" Sys.argv.(0);
    exit 1
  );

  let filename = Sys.argv.(1) in
  let source = read_file filename in

  (* lexer *)
  let tokens = 
    try Lexer.tokenize source
    with
    | Lexer.Error -> Printf.printf "%s\n" msg; exit 1 

  (* parser *)
  let ast = 
    try Parser.parse tokens
    with 
    | Parser.Error msg -> Printf.printf "%s\n" msg; exit 1

  (* checker *)
  let typed = 
    try Checker.check ast
    with
    | Checker.Error msg -> Printf.printf "%s\n" msg; exit 1

  (* codegen *)

  (* vm *)