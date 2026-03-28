(**
  @file   heap.ml
  @brief  Raw word-slab allocator for COOL runtime objects. Manages a
          Bigarray.Array1 of nativeint words that lives outside OCaml's GC.
          Allocation uses a bump pointer with a first-fit free list for
          reclaimed blocks. Provides field encode/decode between the OCaml
          value type and the slab's tagged-word representation, and exposes
          the header manipulation primitives used by the collector.
  @author Trey Rubino
  @date   03/28/2026
*)

open Bigarray
open Runtime
open Error

(* -------------------------------------------------------------------------
   tagged-word constants
   ----------------------------------------------------------------------- *)

let tag_int     = 1n
let tag_bool    = 2n
let tag_ptr     = 3n
let tag_str_idx = 4n
let tag_mask    = 7n

let free_node_hdr = 2n    (* header sentinel for free blocks: bit 1 set *)

(* -------------------------------------------------------------------------
   value <-> slab-word encoding
   ----------------------------------------------------------------------- *)

let encode (v : value) : nativeint =
  match v with
  | VVoid   -> 0n
  | VInt n  -> Nativeint.logor (Nativeint.shift_left (Nativeint.of_int n) 3) tag_int
  | VBool b -> Nativeint.logor (Nativeint.shift_left (Nativeint.of_int (if b then 1 else 0)) 3) tag_bool
  | VPtr p  -> Nativeint.logor (Nativeint.shift_left (Nativeint.of_int p) 3) tag_ptr

let decode (w : nativeint) : value =
  if w = 0n then VVoid
  else
    let tag = Nativeint.to_int (Nativeint.logand w tag_mask) in
    match tag with
    | 1 -> VInt  (Nativeint.to_int (Nativeint.shift_right w 3))
    | 2 -> VBool (Nativeint.to_int (Nativeint.logand w 8n) <> 0)
    | 3 -> VPtr  (Nativeint.to_int (Nativeint.shift_right_logical w 3))
    | _ -> VVoid

(* encode a string-table index into a StrIdx slab word (tag 4).
   this word type is ONLY written into String object field[0] and is
   NEVER placed in the OCaml value type or on the operand stack. *)
let encode_str_idx (i : int) : nativeint =
  Nativeint.logor (Nativeint.shift_left (Nativeint.of_int i) 3) tag_str_idx

(* extract a string-table index from a StrIdx slab word. *)
let decode_str_idx (w : nativeint) : int =
  Nativeint.to_int (Nativeint.shift_right_logical w 3)

(* true if a slab word carries a StrIdx (tag = 4). *)
let is_str_idx (w : nativeint) : bool =
  Nativeint.logand w tag_mask = tag_str_idx

(* true if a slab word carries a VPtr (tag = 3). *)
let is_ptr (w : nativeint) : bool =
  Nativeint.logand w tag_mask = tag_ptr

(* -------------------------------------------------------------------------
   header word helpers — used by allocator and collector
   ----------------------------------------------------------------------- *)

(* build a live-object header from a class id (mark bit clear). *)
let make_header (cid : int) : nativeint =
  Nativeint.shift_left (Nativeint.of_int cid) 2

(* extract class id from a live-object header. *)
let hdr_class_id (h : nativeint) : int =
  Nativeint.to_int (Nativeint.shift_right_logical h 2)

(* true if the GC mark bit (bit 0) is set. *)
let hdr_is_marked (h : nativeint) : bool =
  Nativeint.logand h 1n <> 0n

(* set the GC mark bit. *)
let hdr_set_mark (h : nativeint) : nativeint =
  Nativeint.logor h 1n

(* clear the GC mark bit. *)
let hdr_clear_mark (h : nativeint) : nativeint =
  Nativeint.logand h (Nativeint.lognot 1n)

(* true if this slab position is a free-list node (bit 1 set). *)
let hdr_is_free (h : nativeint) : bool =
  Nativeint.logand h 2n <> 0n

(* -------------------------------------------------------------------------
   object accessors
   ----------------------------------------------------------------------- *)

let class_id (h : heap) (p : int) : int =
  hdr_class_id h.slab.{p}

let total_size (h : heap) (p : int) : int =
  Nativeint.to_int h.slab.{p + 1}

(* read the n-th field of the object at word offset p (0-indexed). *)
let get_field (h : heap) (p : int) (n : int) : value =
  decode h.slab.{p + 2 + n}

(* write the n-th field of the object at word offset p. *)
let set_field (h : heap) (p : int) (n : int) (v : value) : unit =
  h.slab.{p + 2 + n} <- encode v

(* read the string-table index from a String object's sole field. *)
let get_str_field (h : heap) (p : int) : int =
  decode_str_idx h.slab.{p + 2}

(* write the string-table index into a String object's sole field. *)
let set_str_field (h : heap) (p : int) (idx : int) : unit =
  h.slab.{p + 2} <- encode_str_idx idx

(* -------------------------------------------------------------------------
   GC mark helpers — called by the collector
   ----------------------------------------------------------------------- *)

let mark_obj (h : heap) (p : int) : unit =
  h.slab.{p} <- hdr_set_mark h.slab.{p}

let is_marked (h : heap) (p : int) : bool =
  hdr_is_marked h.slab.{p}

(* -------------------------------------------------------------------------
   allocator
   ----------------------------------------------------------------------- *)

(* initialize the header and size words of a freshly claimed block, zero all
   field words so every field starts as VVoid (0n), and account for the new
   words in n_live_words. *)
let write_object (h : heap) (off : int) (cid : int) (size : int) : unit =
  h.n_live_words    <- h.n_live_words + size;
  h.slab.{off}      <- make_header cid;
  h.slab.{off + 1}  <- Nativeint.of_int size;
  for i = 2 to size - 1 do
    h.slab.{off + i} <- 0n
  done

(* true when live object word count has reached the GC trigger threshold.
   n_live_words tracks only words occupied by live objects; it decreases
   during sweep as dead objects are reclaimed. this prevents GC from firing
   on every allocation after the first collection cycle, which would happen
   if the check used the monotonically increasing bump pointer instead. *)
let needs_gc (h : heap) : bool =
  h.n_live_words >= h.threshold

(* allocate an object with the given class id and field count.
   tries the free list first (first-fit), then bumps into unused slab space.
   errors immediately if neither path has enough room. *)
let alloc (h : heap) (cid : int) (n_fields : int) : int =
  let size = 2 + n_fields in

  (* first-fit search through the free list *)
  let rec search acc = function
    | [] -> None
    | (off, sz) :: rest when sz >= size ->
      h.free <- List.rev_append acc rest;
      Some (off, sz)
    | blk :: rest ->
      search (blk :: acc) rest
  in
  (match search [] h.free with
  | Some (off, sz) ->
    let rem = sz - size in
    if rem >= 2 then begin
      (* split: mark the remainder as a free node in the slab.
         the remainder is NOT counted in n_live_words — only live
         objects are. write_object handles the accounting. *)
      let r = off + size in
      h.slab.{r}     <- free_node_hdr;
      h.slab.{r + 1} <- Nativeint.of_int rem;
      h.free <- (r, rem) :: h.free
    end;
    write_object h off cid size;
    off
  | None ->
    (* bump allocate *)
    if h.next + size > h.capacity then
      Error.vm "0" "out of memory: heap exhausted after garbage collection";
    let off = h.next in
    h.next <- h.next + size;
    write_object h off cid size;
    off)