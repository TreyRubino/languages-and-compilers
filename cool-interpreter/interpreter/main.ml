(* 
@author Trey Rubino
@date 09/06/2025
*)

open Ast
open Reader

let main () = 
  begin
    let fname = Sys.argv.(1) in
    let fin = open_in fname in

    let env = load_runtime_env fin in
    close_in fin;

    Printf.printf "classes=%d, impl_classes=%d, parent_pairs=%d\n"
      (Hashtbl.length env.class_map)
      (Hashtbl.length env.impl_map)
      (Hashtbl.length env.parent_map);
  end;;
main ();;