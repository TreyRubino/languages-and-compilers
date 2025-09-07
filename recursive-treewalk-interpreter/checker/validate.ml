(* 
@author Trey Rubino
@date 09/06/2025
*)

open Ast
open Env

let dups_and_base_validation ~base_classes (ast : cool_program) =
  let seen = Hashtbl.create 255 in
  List.iter (fun ((cloc, cname), _inherits, _features) ->
    if List.mem cname base_classes then (
      Printf.printf "ERROR: %s: Type-Check: Redefining base class %s\n" cloc cname; 
      exit 1
    );
    if Hashtbl.mem seen cname then (
      Printf.printf "ERROR: %s: Type-Check: Duplicate class %s\n" cloc cname; 
      exit 1
    );
    Hashtbl.add seen cname true
  ) ast

let parent_validation ~all_classes (ast : cool_program) =
  let forbidden = ["Int"; "Bool"; "String"; "SELF_TYPE"] in  (* IO is allowed *)
  (* local checks *)
  List.iter (fun ((_cloc, cname), inherits, _features) ->
    match inherits with
    | None -> ()
    | Some (iloc, iname) ->
        if iname = cname then (
          Printf.printf "ERROR: %s: Type-Check: Class cannot inherit from itself (%s)\n" iloc cname; 
          exit 1
        );
        if List.mem iname forbidden then (
          Printf.printf "ERROR: %s: Type-Check: Inheriting from forbidden class %s\n" iloc iname; 
          exit 1
        );
        if not (List.mem iname all_classes) then (
          Printf.printf "ERROR: %s: Type-Check: Inheriting from undefined class %s\n" iloc iname; 
          exit 1
        )
  ) ast;
  (* cycle detection / rooting at Object *)
  let find_opt h k = try Some (Hashtbl.find h k) with Not_found -> None in
  let color : (string, int) Hashtbl.t = Hashtbl.create 255 in  
  (* 0/absent=white,1=gray,2=black *)
  let rec dfs c =
    if c = "Object" then false
    else
      match find_opt color c with
      | Some 1 -> true
      | Some 2 -> false
      | _ ->
          Hashtbl.replace color c 1;
          let parent = try Hashtbl.find parent_map c with Not_found -> "Object" in
          let cyc = dfs parent in
          Hashtbl.replace color c 2;
          cyc
  in
  List.iter (fun c ->
    if c <> "Object" && dfs c then (
      Printf.printf "ERROR: 0: Type-Check: Inheritance cycle\n"; 
      exit 1
    )
  ) all_classes

let decl_types_validation ~all_classes (ast : cool_program) = 
  let type_exists t = List.mem t all_classes in
  List.iter (fun ((_cloc, _cname), _inherits, features) ->
    List.iter (function
      | Attribute ((_aloc, _name), (tloc, tname), _init) ->
        if tname = "SELF_TYPE" then (
          Printf.printf "ERROR: %s: Type-Check: SELF_TYPE not allowed as attribute type\n" tloc;
          exit 1
        );
        if not (type_exists tname) then (
          Printf.printf "ERROR: %s: Type-Check: Unknown type %s" tloc tname;
          exit 1
        );
      | Method ((_mloc, _mname), formals, (rtloc, rtype), _body) -> 
        if rtype <> "SELF_TYPE" && not (type_exists rtype) then (
          Printf.printf "ERROR: %s: Type-Check: Unknown return type %s\n" rtloc, rtype;
          exit 1 
        );  
        List.iter (fun ((_floc, _fname), (ftloc, ftname)) -> 
          if ftname = "SELF_TYPE" then (
            Printf.printf "ERROR: %s: Type-Check: SELF_TYPE not allowed as formal type\n" ftloc; 
            exit 1
          );
          if not (type_exists ftname) then (
            Printf.printf "ERROR: %s Type-Check: Unknown type %s\n" ftloc ftname;
            exit 1
          )
        ) formals
    ) features
  ) ast
