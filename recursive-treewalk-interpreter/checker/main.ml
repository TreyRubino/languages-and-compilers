open Ast
open Env
open Reader
open Typecheck
open Writer
open Validate

let main () =
	begin
		let fname = Sys.argv.(1) in
		let fin = open_in fname in

		(* Read AST from .cl-ast *)
		let ast = read_program fin in
		close_in fin;

		(* Class universe *)
		let base_classes = [ "Int"; "String"; "Bool"; "IO"; "Object" ] in
    dups_and_base_validation ~base_classes ast;

		let user_classes = List.map (fun ((_, cname), _, _) -> cname) ast in
		let all_classes = List.sort compare (base_classes @ user_classes) in

		(* Build parent map (pairs also used for output) *)
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

		(* Seed method table *)
		seed_builtins ();
		seed_user_methods ast;
    seed_user_attributes ast;

		(* Type-check attributes and method bodies *)
		List.iter (fun ((_, cname), _inherits, features) ->
			type_check_class cname ((("", cname)), None, features)
		) ast;

		(* Write .cl-type *)
		let outname = Filename.chop_extension fname ^ ".cl-type" in
		write_all outname ast all_classes parent_pairs
	end;;
main ();;
