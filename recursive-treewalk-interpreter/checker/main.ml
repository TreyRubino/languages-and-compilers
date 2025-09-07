open Ast
open Env
open Reader
open Typecheck
open Writer

let main () =
	begin
		let fname = Sys.argv.(1) in
		let fin = open_in fname in

		(* Read AST from .cl-ast *)
		let ast = read_program fin in
		close_in fin;

		(* Class universe *)
		let base_classes = [ "Int"; "String"; "Bool"; "IO"; "Object" ] in

    let seen = Hashtbl.create 255 in
    List.iter (fun ((cloc, cname), _inherits, _features) ->
      (* forbid redefining any base class *)
      if List.mem cname base_classes then (
        Printf.printf "ERROR: %s: Type-Check: Redefining base class %s\n" cloc cname;
        exit 1
      );
      (* forbid duplicate user class names *)
      if Hashtbl.mem seen cname then (
        Printf.printf "ERROR: %s: Type-Check: Duplicate class %s\n" cloc cname;
        exit 1
      );
      Hashtbl.add seen cname true
    ) ast;

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

    (* Basic class checks *)
    let forbidden = ["Int"; "Bool"; "String"; "SELF_TYPE";] in
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
        );
		) ast;

    (* global cycle detecting *)
    let color : (string, int) Hashtbl.t = Hashtbl.create 255 in
    (* color: 0/absent=white, 1=gray, 2=black *)
    let rec dfs (c : string) : bool =
      if c = "Object" then false
      else
        match Hashtbl.find_opt color c with
        | Some 1 -> true (* gray cycle: theres a back edge *)
        | Some 2 -> false (* black already processed and safe *)
        | _ -> (* white unvisited *)
          Hashtbl.replace color c 1;
          let parent = 
            try Hashtbl.find parent_map c
            with Not_found -> "Object"
          in
          let cyc = dfs parent in 
          Hashtbl.replace color c 2;
          cyc
    in
    List.iter (fun c ->
      if c <> "Object" && dfs c then (
        Printf.printf "ERROR: 0: Type-Check: Inheritance cycle\n";
        exit 1
      )
    ) all_classes;

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
