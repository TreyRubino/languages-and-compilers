(**
@file   main.ml
@brief  main entry point for full pipeline
@author Trey Rubino
@date   11/15/2025
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
  let args = Array.to_list Sys.argv in
  let bootstrap_mode = List.mem "-b" args in
  let remaining_args = List.filter (fun a -> a <> "-b" && a <> Sys.argv.(0)) args in

  if List.length remaining_args < 1 then (
    Printf.eprintf "usage: %s [-b] <file>\n" Sys.argv.(0);
    exit 1
  );

  let filename = List.hd remaining_args in
  let semantic_env = 
    if bootstrap_mode then (
      if not (Filename.check_suffix filename ".cl-type") then (
        Printf.eprintf "Error: Bootstrap mode (-b) requires a .cl-type file.\n";
        exit 1
      );

      let ic = open_in filename in
      let env = 
        try Bootstrap.load_bootstrap_env ic
      with exn ->
        close_in ic;
        Printf.eprintf "Bootstrap error: %s\n" (Printexc.to_string exn); 
        exit 1
      in
      close_in ic;
      env
    ) else (
      let source = read_file filename in

      (* parser calls lexer on demand *)
      let lexbuf = Lexing.from_string source in
      let ast =
        try Parser.cool_program Lexer.token lexbuf
        with
        | Error.E e -> Error.print e; exit 1
        | Parsing.Parse_error -> Error.parser lexbuf; exit 1
      in
      try Checker.check ast
      with Error.E e -> Error.print e; exit 1 
    )
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
  Debug.dump_ir "debug.txt" ir;

  (* vm *)
  try 
    ignore (Vm.execute ir)
  with 
  | Error.E e -> Error.print e; exit 1

