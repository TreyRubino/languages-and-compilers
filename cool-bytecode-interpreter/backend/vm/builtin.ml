(*
@author Trey Rubino
@date 12/02/2025
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

let maybe_handle_builtin (st : vm_state) (frame : frame) : value option =
  let m = frame.method_info in 
  match m.name with 
  | "abort" -> 
    Printf.printf "abort\n"; exit 1

  | "copy" ->
    let obj = frame.self_obj in
    let fields = Array.copy obj.fields in
    let new_obj = {
      class_id = obj.class_id;
      fields;
      payload = obj.payload;
      marked = false;
    } in
    st.heap <- new_obj :: st.heap;
    Some (VObj new_obj)

  | "type_name" ->
    let cid = frame.self_obj.class_id in
    let cname = st.ir.classes.(cid).name in
    Some (Alloc.box_string st cname)

  | "out_int" ->
    let v = frame.locals.(0) in
    let i =
      match v with
      | VObj o ->
        (match o.payload with
        | PInt i -> i
        | _ -> Error.vm "0" "out_int expected Int")
      | _ -> Error.vm "0" "out_int expected Int"
    in
    Printf.printf "%d" i;
    flush stdout;
    Some (VObj frame.self_obj)

  | "out_string" ->
    let v = frame.locals.(0) in
    let s =
      match v with
      | VObj o ->
        (match o.payload with
        | PString s -> s
        | _ -> Error.vm "0" "out_string expected String")
      | _ -> Error.vm "0" "out_string expected String"
    in
    Printf.printf "%s" (unescape s);
    Some (VObj frame.self_obj)

  | "in_int" ->
    (try 
      let line = read_line () in
      let clean = String.trim line in
      let i = int_of_string clean in
      Some (Alloc.box_int st i)
    with _ -> 
      Some (Alloc.box_int st 0))

  | "in_string" ->
    let s = read_line () in
    Some (Alloc.box_string st s)

  | "concat" ->
    let arg =
      match frame.locals.(0) with
      | VObj o ->
        (match o.payload with
        | PString s -> s
        | _ -> Error.vm "0" "concat expected String argument")
      | _ ->
        Error.vm "0" "concat expected String argument"
    in
    (match frame.self_obj.payload with
    | PString base ->
      Some (Alloc.box_string st (base ^ arg))
    | _ ->
      Error.vm "0" "concat on non-string")

  | "substr" ->
    let base =
      match frame.self_obj.payload with
      | PString s -> s
      | _ -> Error.vm "0" "substr on non-string"
    in
    let i =
      match frame.locals.(0) with
      | VObj o ->
        (match o.payload with
        | PInt n -> n
        | _ -> Error.vm "0" "substr expected Int index")
      | _ -> Error.vm "0" "substr expected Int index"
    in
    let l =
      match frame.locals.(1) with
      | VObj o ->
        (match o.payload with
        | PInt n -> n
        | _ -> Error.vm "0" "substr expected Int length")
      | _ -> Error.vm "0" "substr expected Int length"
    in
    if i < 0 || l < 0 || i + l > String.length base then
      Error.vm "0" "substr out of range";
    Some (Alloc.box_string st (String.sub base i l))

  | "length" -> 
    let s = 
      match frame.self_obj.payload with
      | PString s -> s
      | _ -> Error.vm "0" "length on non-string"
    in
    Some (Alloc.box_int st (String.length s))
    
  | _ -> None