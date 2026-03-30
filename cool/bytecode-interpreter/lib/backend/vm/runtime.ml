(**
@file   runtime.ml
@brief  Defines the core runtime types used by the VM: the unboxed value
        type, the raw word-slab managed heap, the parallel string table,
        the call frame, and the virtual machine state. Integers and booleans
        are unboxed values that never touch the heap. Only COOL objects
        created with new and string results go to the slab.
@author Trey Rubino
@date   03/28/2026
*)

open Bigarray
open Ir

(* -------------------------------------------------------------------------
   value — the operational value type carried on the OCaml call stack,
   in frame locals, and on the operand stack. VInt and VBool are unboxed
   and never allocated on the managed slab. VPtr carries a word offset into
   the raw slab. VVoid is the null / void sentinel.
   ----------------------------------------------------------------------- *)
type value =
  | VInt  of int
  | VBool of bool
  | VPtr  of int     (* word offset into the raw heap slab *)
  | VVoid

(* -------------------------------------------------------------------------
   heap — the raw word slab managed entirely by our allocator and collector.
   OCaml's GC does not see the object data inside the slab; it only tracks
   the Array1 container. Allocation uses a bump pointer with a first-fit
   free list for reclaimed blocks. Assumes a 64-bit host.

   slab field word encoding (nativeint):
     0n              = VVoid
     (n lsl 3) | 1n  = VInt n    arithmetic shift on decode, sign-safe
     (b lsl 3) | 2n  = VBool b   b = 0 or 1
     (p lsl 3) | 3n  = VPtr p    heap word offset
     (i lsl 3) | 4n  = StrIdx i  string table index, only in String.field[0]

   object layout at word offset p:
     slab[p+0]              header  = (class_id lsl 2) lor mark_bit
     slab[p+1]              size    = total words including this header (2+n)
     slab[p+2 .. p+1+n]     fields  encoded as above

   free-node layout at word offset p:
     slab[p+0]              2n      free-node sentinel (bit 1 set)
     slab[p+1]              total words in this free block
   ----------------------------------------------------------------------- *)
type heap = {
  slab               : (nativeint, nativeint_elt, c_layout) Array1.t;
  mutable next       : int;              (* bump pointer: next free word offset *)
  mutable free       : (int * int) list; (* (offset, total_words) free blocks   *)
  mutable n_live_words : int;            (* words occupied by live objects only  *)
  capacity           : int;              (* total words in slab                  *)
  threshold          : int;              (* GC trigger when n_live_words >= this *)
}

(* -------------------------------------------------------------------------
   strings — parallel string table. string bytes live in OCaml-managed
   memory while the slab carries only an integer index per String object.
   the collector marks live slots; sweep reclaims dead ones.
   ----------------------------------------------------------------------- *)
type strings = {
  data           : string array;
  mutable live   : bool array;       (* GC mark bitmap, reset each cycle *)
  mutable free   : int list;         (* free slot indices *)
  mutable n_live : int;
  capacity       : int;
  tbl            : (string, int) Hashtbl.t;  (* content -> slot index *)
}

(* -------------------------------------------------------------------------
   frame — a single activation record. self_ptr is the word offset of the
   receiver in the slab, replacing the old self_obj : obj direct reference.
   ----------------------------------------------------------------------- *)
type frame = {
  mutable pc  : int;
  method_info : Ir.method_info;
  locals      : value array;
  self_ptr    : int;               (* word offset of self in slab *)
}

(* -------------------------------------------------------------------------
   vm_state
   ----------------------------------------------------------------------- *)
type vm_state = {
  ir             : Ir.ir;
  mutable stack  : value list;
  mutable frames : frame list;
  heap           : heap;
  strings        : strings;
}

(* -------------------------------------------------------------------------
   construction
   ----------------------------------------------------------------------- *)

let heap_capacity  = 1048576                    (* 1M words, 8MB on 64-bit *)
let heap_threshold = heap_capacity * 3 / 4      (* GC at 75% live usage    *)
let str_capacity   = 65536

let create_heap () : heap =
  let slab = Array1.create nativeint c_layout heap_capacity in
  Array1.fill slab 0n;
  {
    slab;
    next         = 0;
    free         = [];
    n_live_words = 0;
    capacity     = heap_capacity;
    threshold    = heap_threshold;
  }

let create_strings () : strings =
  {
    data     = Array.make str_capacity "";
    live     = Array.make str_capacity false;
    free     = List.init str_capacity (fun i -> i);
    n_live   = 0;
    capacity = str_capacity;
    tbl      = Hashtbl.create 512;
  }

let create_vm (ir : Ir.ir) : vm_state =
  {
    ir;
    stack   = [];
    frames  = [];
    heap    = create_heap ();
    strings = create_strings ();
  }

let string_of_value = function
  | VVoid   -> "void"
  | VInt i  -> Printf.sprintf "Int(%d)" i
  | VBool b -> Printf.sprintf "Bool(%b)" b
  | VPtr p  -> Printf.sprintf "Ptr(%d)" p