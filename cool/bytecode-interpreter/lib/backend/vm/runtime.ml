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

(** @brief Represents the operational value type in the VM. 
           VInt and VBool are unboxed (stored directly as data), 
           VPtr represents a raw word offset into the heap slab, 
           and VVoid acts as the null sentinel. 
    @return A value variant used on the VM stack and in local variables. *)
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

(** @brief Defines the managed heap slab. This structure wraps a Bigarray 
           allocated outside the OCaml GC's reach. It tracks the next 
           available memory offset (bump pointer), a list of reclaimed 
           memory blocks (free list), and the current word-count of 
           live objects to trigger garbage collection.
    @param slab The raw 64-bit word array.
    @param threshold The live-word limit that initiates a GC cycle. *)
type heap = {
  slab               : (nativeint, nativeint_elt, c_layout) Array1.t;
  mutable next       : int;              (* bump pointer: next free word offset *)
  mutable free       : (int * int) list; (* (offset, total_words) free blocks   *)
  mutable n_live_words : int;            (* words occupied by live objects only  *)
  capacity           : int;              (* total words in slab                  *)
  threshold          : int;              (* GC trigger when n_live_words >= this *)
}

(** @brief Manages a parallel string table where raw string bytes are stored 
           in OCaml-managed memory. This prevents variable-length string data 
           from fragmenting the fixed-word heap slab. It uses a hash table 
           for content deduplication (interning) and a bitmap for GC marking.
    @param tbl A mapping from string content to its unique integer slot index. *)
type strings = {
  data           : string array;
  mutable live   : bool array;       (* GC mark bitmap, reset each cycle *)
  mutable free   : int list;         (* free slot indices *)
  mutable n_live : int;
  capacity       : int;
  tbl            : (string, int) Hashtbl.t;  (* content -> slot index *)
}

(** @brief Represents a single activation record (call frame) on the VM stack. 
           It stores the current program counter, method metadata, local 
           variable array, and the slab offset of the 'self' object receiver.
    @param self_ptr The memory offset in the slab where the receiver object resides. *)
type frame = {
  mutable pc  : int;
  method_info : Ir.method_info;
  locals      : value array;
  self_ptr    : int;               (* word offset of self in slab *)
}

(** @brief The global state of the Virtual Machine, encapsulating the 
           intermediate representation (IR), operand stack, call frame 
           hierarchy, and the dual-managed memory systems (heap and strings). *)
type vm_state = {
  ir             : Ir.ir;
  mutable stack  : value list;
  mutable frames : frame list;
  heap           : heap;
  strings        : strings;
}

let heap_capacity  = 1048576                    (* 1M words, 8MB on 64-bit *)
let heap_threshold = heap_capacity * 3 / 4      (* GC at 75% live usage    *)
let str_capacity   = 65536

(** @brief Allocates and initializes the raw word slab using C-layout memory. 
           All words are initially zeroed to ensure VVoid consistency.
    @return An initialized heap record with a 1M word capacity. *)
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

(** @brief Allocates the structures required for the parallel string table, 
           initializing a free-list of indices and an empty interning hash table.
    @return An initialized strings record with a 64k slot capacity. *)
let create_strings () : strings =
  {
    data     = Array.make str_capacity "";
    live     = Array.make str_capacity false;
    free     = List.init str_capacity (fun i -> i);
    n_live   = 0;
    capacity = str_capacity;
    tbl      = Hashtbl.create 512;
  }

(** @brief Constructs the initial VM state by linking the provided IR 
           definitions with freshly allocated heap and string management systems.
    @param ir The intermediate representation produced by the compiler.
    @return A clean vm_state ready for program execution. *)
let create_vm (ir : Ir.ir) : vm_state =
  {
    ir;
    stack   = [];
    frames  = [];
    heap    = create_heap ();
    strings = create_strings ();
  }

(** @brief Converts a VM value variant into a human-readable string representation 
           for debugging and diagnostic logging.
    @param v The value to convert.
    @return A string describing the value type and its contents. *)
let string_of_value = function
  | VVoid   -> "void"
  | VInt i  -> Printf.sprintf "Int(%d)" i
  | VBool b -> Printf.sprintf "Bool(%b)" b
  | VPtr p  -> Printf.sprintf "Ptr(%d)" p