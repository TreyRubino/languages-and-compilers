(*
@author Trey Rubino
@date 11/14/2025
*)

open Ast
open Env
open Typecheck
open Validate

exception Error of string

let check (ast : Ast.cool_program) = 
  (
		let base_classes = [ "Int"; "String"; "Bool"; "IO"; "Object" ] in
    dups_base_validation ~base_classes ast;

		let user_classes = List.map (fun ((_, cname), _, _) -> cname) ast in
		let all_classes = List.sort compare (base_classes @ user_classes) in

		let parent_pairs =
			let builtins = [
				("Bool",   "Object");
				("IO",     "Object");
				("Int",    "Object");
				("String", "Object");
			] in
			let user_pairs =
				List.map (fun ((_, cname), inherits, _) ->
					match inherits with
					| None -> (cname, "Object")
					| Some ((_, pname)) -> (cname, pname)
				) ast
			in
			builtins @ user_pairs
		in
		Hashtbl.clear parent_map;
		List.iter (fun (c, p) -> Hashtbl.replace parent_map c p) parent_pairs;

    parent_validation ~all_classes ast;
    decl_types_validation ~all_classes ast;
    main_validation ast;

		seed_builtins ();

		seed_user_methods ast;
    override_validation ast;

    seed_user_attributes ast;
    names_scoping_validation ast;

		List.iter (fun ((_, cname), _inherits, features) ->
			type_check_class cname ((("", cname)), None, features)
		) ast;
  );;