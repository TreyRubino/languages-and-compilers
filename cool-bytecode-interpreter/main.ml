(*
@author Trey Rubino
@date 11/13/2025
*)

Printexc.record_backtrace true;;

open Debug

let read_file filename =
  let ic = open_in filename in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  s

let () =
  if Array.length Sys.argv < 2 then (
    Printf.eprintf "usage: %s <file>\n" Sys.argv.(0);
    exit 1
  );

  let filename = Sys.argv.(1) in
  let source = read_file filename in

  (* parser calls lexer on demand *)
  let lexbuf = Lexing.from_string source in
  let ast =
    try Parser.cool_program Lexer.token lexbuf
    with
    | Error.E e -> Error.print e; exit 1
    | Parsing.Parse_error -> Error.parser lexbuf; exit 1
  in

  (* type check *)
  let semantic_env =
    try Checker.check ast
    with Error.E e -> Error.print e; exit 1
  in

  (* compile *)
  let ir =
    try Codegen.emit semantic_env
    with
    | Error.E e -> Error.print e; exit 1
    | exn ->
      Printf.printf "Unexpected exception: %s\n%!" (Printexc.to_string exn);
      Printf.printf "%s\n%!" (Printexc.get_backtrace ());
      exit 1
  in

  (* debugging *)
  (*Debug.dump_ir "debug.txt" ir;*)

  (* vm *)
  try 
    ignore (Vm.execute ir)
  with 
  | Error.E e -> Error.print e; exit 1

