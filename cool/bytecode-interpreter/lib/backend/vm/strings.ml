(**
@file   strings.ml
@brief  Parallel string table for COOL runtime strings. String bytes live
        in an OCaml string array managed here. The slab carries only an
        integer slot index per String object. intern deduplicates content
        via a hash table. The collector marks live slots; sweep reclaims
        unreachable ones back to the free list.
@author Trey Rubino
@date   03/28/2026
*)

open Runtime
open Error

(* intern a string: return its existing slot index if already present,
   otherwise claim a free slot, record the content, and return the index.
   errors immediately if the string table is exhausted. *)
let intern (st : strings) (s : string) : int =
  match Hashtbl.find_opt st.tbl s with
  | Some i -> i
  | None ->
    (match st.free with
    | [] ->
      Error.vm "0" "string table exhausted"
    | i :: rest ->
      st.data.(i)  <- s;
      st.live.(i)  <- false;
      st.free      <- rest;
      st.n_live    <- st.n_live + 1;
      Hashtbl.add st.tbl s i;
      i)

(* mark a string slot as reachable during the GC mark phase. *)
let mark (st : strings) (i : int) : unit =
  st.live.(i) <- true

(* sweep the string table: reclaim every slot that was not marked.
   only reclaims slot i if the deduplication table maps data.(i) back
   to exactly i — this prevents uninitialized slots (whose data is "")
   from being confused with the canonical empty-string slot. live slots
   have their mark bit reset for the next cycle. *)
let sweep (st : strings) : unit =
  for i = 0 to st.capacity - 1 do
    if st.live.(i) then
      st.live.(i) <- false
    else if Hashtbl.find_opt st.tbl st.data.(i) = Some i then begin
      Hashtbl.remove st.tbl st.data.(i);
      st.data.(i)  <- "";
      st.free      <- i :: st.free;
      st.n_live    <- st.n_live - 1
    end
  done