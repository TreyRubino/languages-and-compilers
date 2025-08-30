(* @author Trey Rubino *)

open Ast
open Env

type object_env = (string, static_type) Hashtbl.t
let empty_object_env () = Hashtbl.create 255

let rec lub (t1:static_type) (t2:static_type) (self_cls:string) : static_type =
	let norm = function SELF_TYPE _ -> Class self_cls | x -> x in
	match norm t1, norm t2 with
	| Class a, Class b ->
		let ancestors x =
			let rec up c acc =
				if c = "Object" then "Object" :: acc
				else
					let p = try Hashtbl.find parent_map c with Not_found -> "Object" in
					up p (c :: acc)
			in
			up x []
		in
		let as1 = ancestors a in
		let as2 = ancestors b in
		let rec meet l1 l2 last =
			match l1, l2 with
			| x::xs, y::ys when x = y -> meet xs ys x
			| _ , _ -> (match last with None -> Class "Object" | Some c -> Class c)
		in
		let rec drop_common_prefix l1 l2 =
			match l1, l2 with
			| x::xs, y::ys when x = y -> drop_common_prefix xs ys
			| _ -> (l1, l2)
		in
		let a1, a2 = drop_common_prefix as1 as2 in
		(* walk from Object up, keep last common *)
		let rec from_root l =
			match l with
			| [] -> None
			| _ -> Some "Object"
		in
		(* simpler: climb a until some ancestor is ancestor of b *)
		let rec climb c =
			let rec is_ancestor d =
				if d = c then true
				else if d = "Object" then c = "Object"
				else
					let p = try Hashtbl.find parent_map d with Not_found -> "Object" in
					is_ancestor p
			in
			if is_ancestor b then Class b
			else if c = "Object" then Class "Object"
			else
				let p = try Hashtbl.find parent_map c with Not_found -> "Object" in
				climb p
		in
		(* do symmetric: climb a until it's ancestor of b *)
		let rec is_ancestor cand x =
			if x = cand then true
			else if x = "Object" then cand = "Object"
			else
				let p = try Hashtbl.find parent_map x with Not_found -> "Object" in
				is_ancestor cand p
		in
		let rec climb_a ca =
			if is_ancestor ca b then Class ca
			else
				let p = try Hashtbl.find parent_map ca with Not_found -> "Object" in
				if ca = "Object" then Class "Object" else climb_a p
		in
		climb_a a
	| _ -> Class "Object"

let rec type_check (current_class:string) (o:object_env) (expr:expr) : static_type =
	let check (exprs:expr list) (expected:string) =
		List.iter (fun e ->
			match type_check current_class o e with
			| Class t when t = expected -> ()
			| Class _ -> raise TYPE_ERROR
			| SELF_TYPE _ ->
				(* SELF_TYPE not allowed for Int/String/Bool ops *)
				if expected = "Int" || expected = "Bool" || expected = "String"
				then raise TYPE_ERROR
		) exprs ;
		Class expected
	in
	let st =
		match expr.expr_kind with
		| Integer _ -> Class "Int"
		| String _ -> Class "String"
		| True | False -> Class "Bool"

		| Identifier ((vloc, vname)) ->
			if Hashtbl.mem o vname then Hashtbl.find o vname
			else (Printf.printf "ERROR: %s: Type-Check: Undeclared variable %s\n" vloc vname ; exit 1)

		| Assign ((vloc, vname), e) ->
			if Hashtbl.mem o vname then
				let declared = Hashtbl.find o vname in
				let et = type_check current_class o e in
				if is_subtype et declared then et
				else raise TYPE_ERROR
			else (Printf.printf "ERROR: %s: Type-Check: Undeclared variable %s\n" vloc vname ; exit 1)

		| Plus (x,y)  -> check [x;y] "Int"
		| Minus (x,y) -> check [x;y] "Int"
		| Times (x,y) -> check [x;y] "Int"
		| Divide (x,y)-> check [x;y] "Int"
		| Tilde x     -> check [x]   "Int"
		| Lt (x,y)    -> ignore (check [x;y] "Int"); Class "Bool"
		| Le (x,y)    -> ignore (check [x;y] "Int"); Class "Bool"

		| Equals (x,y) ->
			let t1 = type_check current_class o x in
			let t2 = type_check current_class o y in
			if is_primitive t1 || is_primitive t2 then
				if t1 <> t2 then raise TYPE_ERROR ;
			Class "Bool"

		| Not e1 ->
			ignore (check [e1] "Bool"); Class "Bool"

		| Isvoid e1 ->
			ignore (type_check current_class o e1); Class "Bool"

		| If (p, tbr, ebr) ->
			let pt = type_check current_class o p in
			if pt <> Class "Bool" then raise TYPE_ERROR ;
			let tt = type_check current_class o tbr in
			let et = type_check current_class o ebr in
			lub tt et current_class

		| While (p, b) ->
			let pt = type_check current_class o p in
			if pt <> Class "Bool" then raise TYPE_ERROR ;
			ignore (type_check current_class o b) ;
			Class "Object"

		| Let ((vloc,vname), (_tloc,tname), init_opt, body) ->
			let declared = if tname = "SELF_TYPE" then SELF_TYPE current_class else Class tname in
			Hashtbl.add o vname declared ;
			(match init_opt with
			| None -> ()
			| Some init ->
				let it = type_check current_class o init in
				if not (is_subtype it declared) then raise TYPE_ERROR) ;
			let bt = type_check current_class o body in
			Hashtbl.remove o vname ;
			bt

		| New ((_, tname)) ->
			if tname = "SELF_TYPE" then SELF_TYPE current_class else Class tname

		| SelfDispatch ((_, mname), args) ->
			let recv_cls = current_class in
			(match lookup_method_sig recv_cls mname with
			| None ->
				Printf.printf "ERROR: %s: Type-Check: Unknown method %s on %s\n" expr.loc mname recv_cls ; exit 1
			| Some sig_ ->
				if List.length sig_.formals <> List.length args then raise TYPE_ERROR ;
				List.iter2 (fun a ft ->
					let at = type_check current_class o a in
					if not (is_subtype at (Class ft)) then raise TYPE_ERROR
				) args sig_.formals ;
				if sig_.ret = "SELF_TYPE" then SELF_TYPE recv_cls else Class sig_.ret)

		| DynamicDispatch (recv, (_, mname), args) ->
			let rt = type_check current_class o recv in
			let (rc : string) =
				match rt with
				| SELF_TYPE c -> c
				| Class c -> c
			in
			(match lookup_method_sig rc mname with
			| None ->
				Printf.printf "ERROR: %s: Type-Check: Unknown method %s on %s\n" expr.loc mname rc ; exit 1
			| Some sig_ ->
				if List.length sig_.formals <> List.length args then raise TYPE_ERROR ;
				List.iter2 (fun a ft ->
					let at = type_check current_class o a in
					if not (is_subtype at (Class ft)) then raise TYPE_ERROR
				) args sig_.formals ;
				(match sig_.ret with
				| "SELF_TYPE" ->
					(* result type is the dynamic receiver type *)
					rt
				| r -> Class r))

		| StaticDispatch (recv, (_tl, tname), (_ml, mname), args) ->
			let rt = type_check current_class o recv in
			let ann = tname in
			(* receiver must be subtype of annotated type *)
			if not (is_subtype rt (Class ann)) then raise TYPE_ERROR ;
			(match lookup_method_sig ann mname with
			| None ->
				Printf.printf "ERROR: %s: Type-Check: Unknown method %s on %s\n" expr.loc mname ann ; exit 1
			| Some sig_ ->
				if List.length sig_.formals <> List.length args then raise TYPE_ERROR ;
				List.iter2 (fun a ft ->
					let at = type_check current_class o a in
					if not (is_subtype at (Class ft)) then raise TYPE_ERROR
				) args sig_.formals ;
				if sig_.ret = "SELF_TYPE" then
					(* static dispatch to A::m has SELF_TYPE meaning A *)
					Class ann
				else Class sig_.ret)
	in
	expr.static_type <- Some st ;
	st

let type_check_class (cname:string) ((_id, _), _inherits, features) =
	(* build object env with self + formals per method when checking *)
	List.iter (fun feat ->
		match feat with
		| Attribute ((_aloc,_aname), (_tl, _tname), init_opt) ->
			(* check attribute init if present in empty env with self available *)
			(match init_opt with
			| None -> ()
			| Some e ->
				let o = empty_object_env () in
				Hashtbl.add o "self" (SELF_TYPE cname) ;
				ignore (type_check cname o e))
		| Method ((_mloc, _mname), formals, (_rtl, _rtype), body) ->
			let o = empty_object_env () in
			Hashtbl.add o "self" (SELF_TYPE cname) ;
			List.iter (fun ((_fl,fname), (_tl,tname)) ->
				Hashtbl.add o fname (if tname = "SELF_TYPE" then SELF_TYPE cname else Class tname)
			) formals ;
			ignore (type_check cname o body)
	) features
