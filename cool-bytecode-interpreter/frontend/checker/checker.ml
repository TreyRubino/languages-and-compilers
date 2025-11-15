(*
@author Trey Rubino
@date 11/14/2025
*)

open Ast
open Env
open Typecheck
open Validate
open Semantics

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

    let env = Semantics.empty_env () in

    Hashtbl.iter (fun cls attrs_tbl ->
      let attrs =
        Hashtbl.fold (fun name ty acc ->
          let init =
            match List.find_opt (function
              | Attribute ((_, aname), _, _) when aname = name -> true
              | _ -> false
            ) (List.concat (List.map (fun ((_, c), _, f) -> if c = cls then f else []) ast)) with
            | Some (Attribute (_, _, init_opt)) -> init_opt
            | _ -> None
          in
          { aname = name; atype = ty; init } :: acc
        ) attrs_tbl []
      in
      Hashtbl.replace env.class_map cls attrs
    ) attribute_env;

    Hashtbl.iter (fun cls methods_tbl ->
      let tbl = Hashtbl.create 31 in
      Hashtbl.iter (fun mname sig_ ->
        let body =
          match List.find_opt (function
            | Method ((_loc, n), _, _, _) when n = mname -> true
            | _ -> false
          ) (List.concat (List.map (fun ((_, c), _, f) -> if c = cls then f else []) ast)) with
          | Some (Method (_, _, _, b)) -> User b
          | _ -> Internal { rtype = sig_.ret; qname = cls ^ "." ^ mname }
        in
        let impl = {
          definer = sig_.definer;
          formals = sig_.formals;
          body;
        } in
        Hashtbl.replace tbl mname impl
      ) methods_tbl;
      Hashtbl.replace env.impl_map cls tbl
    ) method_env;

    Hashtbl.iter (fun c p ->
      Hashtbl.replace env.parent_map c p
    ) parent_map;

    env
  );;