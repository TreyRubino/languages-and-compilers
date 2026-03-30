(**
@file   gc.ml
@brief  Mark-and-sweep garbage collector for the raw word slab and the
        parallel string table. The mark phase uses an explicit worklist to
        avoid OCaml call-stack overflow on deep object graphs. It traces
        all VPtr and StrIdx references reachable from the operand stack,
        active frame locals, and frame self pointers, plus all constant
        strings that are permanently live. The sweep phase scans the slab
        linearly, coalescing adjacent dead blocks into the free list, and
        then sweeps the string table.
@author Trey Rubino
@date   03/28/2026
*)

open Runtime

(* -------------------------------------------------------------------------
   mark phase
   ----------------------------------------------------------------------- *)

(* seed the worklist from an OCaml value; only VPtr references slab objects. *)
let seed_value (worklist : int Queue.t) (v : value) : unit =
  match v with
  | VPtr p -> Queue.push p worklist
  | _      -> ()

(* scan a single slab object's fields, pushing child pointers onto the
   worklist and directly marking any StrIdx references found. *)
let scan_fields (worklist : int Queue.t) (st : vm_state) (p : int) : unit =
  let size = Nativeint.to_int st.heap.slab.{p + 1} in
  for i = 2 to size - 1 do
    let w = st.heap.slab.{p + i} in
    if w <> 0n then
      if Heap.is_ptr w then
        Queue.push (Nativeint.to_int (Nativeint.shift_right_logical w 3)) worklist
      else if Heap.is_str_idx w then
        Strings.mark st.strings (Heap.decode_str_idx w)
  done

(* mark all objects reachable from the given roots using a worklist.
   the mark bit in the slab header is the visited flag; objects already
   marked are skipped to handle cycles and shared structure. *)
let mark_roots (st : vm_state) : unit =
  let worklist = Queue.create () in

  (* operand stack *)
  List.iter (seed_value worklist) st.stack;

  (* active frames: self pointer and all locals *)
  List.iter (fun f ->
    Queue.push f.self_ptr worklist;
    Array.iter (seed_value worklist) f.locals
  ) st.frames;

  (* constant-table strings are permanently live for the program lifetime *)
  Array.iter (function
    | Ir.LString s ->
      (match Hashtbl.find_opt st.strings.tbl s with
      | Some i -> Strings.mark st.strings i
      | None   -> ())
    | _ -> ()
  ) st.ir.consts;

  (* drain worklist *)
  while not (Queue.is_empty worklist) do
    let p = Queue.pop worklist in
    if not (Heap.is_marked st.heap p) then begin
      Heap.mark_obj st.heap p;
      scan_fields worklist st p
    end
  done

(* -------------------------------------------------------------------------
   sweep phase
   ----------------------------------------------------------------------- *)

(* scan the slab linearly from offset 0 to the bump pointer.
   three cases per block:
     pre-existing free node   — already reclaimed; just re-add to free list
                                for coalescing. n_live_words unchanged.
     unmarked live object     — newly dead; deduct from n_live_words, convert
                                to free node, coalesce with previous if adjacent.
     marked live object       — survives; clear mark bit for next cycle.
   rebuilds h.free from scratch each collection. *)
let sweep_heap (st : vm_state) : unit =
  let h = st.heap in
  h.free <- [];
  let i = ref 0 in
  while !i < h.next do
    let header = h.slab.{!i} in
    let size   = Nativeint.to_int h.slab.{!i + 1} in

    if Heap.hdr_is_free header then begin
      (* pre-existing free node: coalesce into free list, no live_words change *)
      (match h.free with
      | (prev_off, prev_sz) :: rest when prev_off + prev_sz = !i ->
        let merged = prev_sz + size in
        h.slab.{prev_off + 1} <- Nativeint.of_int merged;
        h.free <- (prev_off, merged) :: rest
      | _ ->
        h.free <- (!i, size) :: h.free)

    end else if not (Heap.hdr_is_marked header) then begin
      (* newly dead live object: deduct its words and add to free list *)
      h.n_live_words <- h.n_live_words - size;
      h.slab.{!i} <- Heap.free_node_hdr;
      (match h.free with
      | (prev_off, prev_sz) :: rest when prev_off + prev_sz = !i ->
        let merged = prev_sz + size in
        h.slab.{prev_off + 1} <- Nativeint.of_int merged;
        h.free <- (prev_off, merged) :: rest
      | _ ->
        h.free <- (!i, size) :: h.free)

    end else begin
      (* live: clear mark bit for next cycle *)
      h.slab.{!i} <- Heap.hdr_clear_mark header
    end;

    i := !i + size
  done

(* -------------------------------------------------------------------------
   collection entry point
   ----------------------------------------------------------------------- *)

let collect (st : vm_state) : unit =
  mark_roots st;
  sweep_heap st;
  Strings.sweep st.strings