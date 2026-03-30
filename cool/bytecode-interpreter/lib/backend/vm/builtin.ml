(**
@file   builtin.ml
@brief  Handles all built-in COOL methods such as abort, copy, type_name,
        I/O routines, and string operations. Dispatches these before normal
        bytecode execution. Integer and boolean return values are unboxed.
        String content is read from the parallel string table via the
        StrIdx stored in String object field[0].
@author Trey Rubino
@date   12/02/2025
*)

open Runtime
open Bytecode
open Error

let unescape (s : string) : string =
  let len = String.length s in
  let buf = Buffer.create len in
  let rec loop i =
    if i >= len then ()
    else
      match s.[i] with
      | '\\' when i + 1 < len ->
        (match s.[i+1] with
        | 'n'  -> Buffer.add_char buf '\n'  ; loop (i+2)
        | 't'  -> Buffer.add_char buf '\t'  ; loop (i+2)
        | '"'  -> Buffer.add_char buf '\\'; Buffer.add_char buf '"'; loop (i+2)
        | c    ->
          Buffer.add_char buf '\\';
          Buffer.add_char buf c;
          loop (i+2))
      | c ->
        Buffer.add_char buf c; loop (i+1)
  in
  loop 0;
  Buffer.contents buf

(* retrieve the string content of the String object at slab offset ptr. *)
let get_string (st : vm_state) (ptr : int) : string =
  let idx = Heap.get_str_field st.heap ptr in
  st.strings.data.(idx)

let maybe_handle_builtin (st : vm_state) (frame : frame) : value option =
  let m = frame.method_info in
  match m.name with

  | "abort" ->
    Printf.printf "abort\n"; exit 1

  | "copy" ->
    if Heap.needs_gc st.heap then Gc.collect st;
    let p    = frame.self_ptr in
    let cid  = Heap.class_id st.heap p in
    let size = Heap.total_size st.heap p in
    let new_p = Heap.alloc st.heap cid (size - 2) in
    (* copy all field words verbatim, including any StrIdx in String objects *)
    for i = 2 to size - 1 do
      st.heap.slab.{new_p + i} <- st.heap.slab.{p + i}
    done;
    Some (VPtr new_p)

  | "type_name" ->
    let cid   = Heap.class_id st.heap frame.self_ptr in
    let cname = st.ir.classes.(cid).name in
    Some (VPtr (Alloc.allocate_string st cname))

  | "out_int" ->
    let i =
      match frame.locals.(0) with
      | VInt i -> i
      | _      -> Error.vm "0" "out_int expected Int"
    in
    Printf.printf "%d" i;
    flush stdout;
    Some (VPtr frame.self_ptr)

  | "out_string" ->
    let s =
      match frame.locals.(0) with
      | VPtr p -> get_string st p
      | _      -> Error.vm "0" "out_string expected String"
    in
    Printf.printf "%s" (unescape s);
    flush stdout;
    Some (VPtr frame.self_ptr)

  | "in_int" ->
    let i =
      try int_of_string (String.trim (read_line ()))
      with _ -> 0
    in
    Some (VInt i)

  | "in_string" ->
    let s = read_line () in
    Some (VPtr (Alloc.allocate_string st s))

  | "concat" ->
    let base = get_string st frame.self_ptr in
    let arg  =
      match frame.locals.(0) with
      | VPtr p -> get_string st p
      | _      -> Error.vm "0" "concat expected String argument"
    in
    Some (VPtr (Alloc.allocate_string st (base ^ arg)))

  | "substr" ->
    let base = get_string st frame.self_ptr in
    let i =
      match frame.locals.(0) with
      | VInt n -> n
      | _      -> Error.vm "0" "substr expected Int index"
    in
    let l =
      match frame.locals.(1) with
      | VInt n -> n
      | _      -> Error.vm "0" "substr expected Int length"
    in
    if i < 0 || l < 0 || i + l > String.length base then
      Error.vm "0" "substr out of range";
    Some (VPtr (Alloc.allocate_string st (String.sub base i l)))

  | "length" ->
    let s = get_string st frame.self_ptr in
    Some (VInt (String.length s))

  | _ -> None