(*
@author Trey Rubino
@date 11/13/2025
*)

Printexc.record_backtrace true;;

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

  let lexbuf = Lexing.from_string source in
  let ast =
    try Parser.cool_program Lexer.token lexbuf
    with
    | Error.E e -> Error.print e; exit 1
    | Parsing.Parse_error -> Error.parser lexbuf; exit 1
  in

  let semantic_env =
    try Checker.check ast
    with Error.E e -> Error.print e; exit 1
  in

  let bytecode =
    try Codegen.emit semantic_env
    with
    | Error.E e -> Error.print e; exit 1
    | exn ->
        Printf.printf "Unexpected exception: %s\n%!" (Printexc.to_string exn);
        Printf.printf "%s\n%!" (Printexc.get_backtrace ());
        exit 1
  in

  Printf.printf "\n--- IR.consts ---\n%!";
  Array.iteri (fun i c ->
    match c with
    | Ir.LInt n -> Printf.printf "%d: int %d\n%!" i n
    | Ir.LBool b -> Printf.printf "%d: bool %b\n%!" i b
    | Ir.LString s -> Printf.printf "%d: string \"%s\"\n%!" i s
    | Ir.LVoid -> Printf.printf "%d: void\n%!" i
  ) bytecode.consts;

  Printf.printf "\n--- IR.classes ---\n%!";
  Array.iter (fun (cls : Ir.class_info) ->
    Printf.printf "class %s (id=%d parent=%d)\n%!" cls.name cls.id cls.parent_id;
    Array.iter (fun (a : Ir.attr_info) ->
      Printf.printf "  attr %s @%d\n%!" a.name a.offset
    ) cls.attributes;
    Printf.printf "  dispatch size=%d\n%!" (Array.length cls.dispatch)
  ) bytecode.classes;

  Printf.printf "\n--- IR.methods ---\n%!";
  Array.iteri (fun i (m : Ir.method_info) ->
    Printf.printf "method[%d] %s (class=%d formals=%d locals=%d)\n%!"
      i m.name m.class_id m.n_formals m.n_locals;
    Printf.printf "  code size=%d\n%!" (Array.length m.code)
  ) bytecode.methods;

  Printf.printf "\nentry_method = %d\n%!" bytecode.entry_method;

  Printf.printf "\n--- end IR dump ---\n%!";
  ()
