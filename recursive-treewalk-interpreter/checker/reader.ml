(* @author Trey Rubino *)

open Ast

let rec range k = if k <= 0 then [] else k :: (range (k - 1))
let read ic = input_line ic

let read_list ic worker =
	let k = int_of_string (read ic) in
	List.map (fun _ -> worker ic) (range k)

let rec read_program ic = read_list ic read_cool_class

and read_id ic =
	let loc = read ic in
	let name = read ic in
	(loc, name)

and read_cool_class ic =
	let cname = read_id ic in
	let inherits =
		match read ic with
		| "no_inherits" -> None
		| "inherits" -> Some (read_id ic)
		| x -> failwith ("bad inherits tag: " ^ x)
	in
	let features = read_list ic read_feature in
	(cname, inherits, features)

and read_feature ic =
	match read ic with
	| "attribute_no_init" ->
		Attribute(read_id ic, read_id ic, None)
	| "attribute_init" ->
		Attribute(read_id ic, read_id ic, Some (read_expr ic))
	| "method" ->
		let mname = read_id ic in
		let formals = read_list ic read_formal in
		let mtype = read_id ic in
		let mbody = read_expr ic in
		Method(mname, formals, mtype, mbody)
	| x -> failwith ("bad feature tag: " ^ x)

and read_formal ic =
	let fname = read_id ic in
	let ftype = read_id ic in
	(fname, ftype)

and read_expr ic =
	let eloc = read ic in
	let ekind =
		match read ic with
		| "assign" ->
			let lh = read_id ic in
			let rh = read_expr ic in
			Assign(lh, rh)
		| "dynamic_dispatch" ->
			let recv = read_expr ic in
			let m = read_id ic in
			let args = read_list ic read_expr in
			DynamicDispatch(recv, m, args)
		| "static_dispatch" ->
			let recv = read_expr ic in
			let ty = read_id ic in
			let m = read_id ic in
			let args = read_list ic read_expr in
			StaticDispatch(recv, ty, m, args)
		| "self_dispatch" ->
			let m = read_id ic in
			let args = read_list ic read_expr in
			SelfDispatch(m, args)
		| "if" ->
			let p = read_expr ic in
			let t = read_expr ic in
			let e = read_expr ic in
			If(p, t, e)
		| "while" ->
			let p = read_expr ic in
			let b = read_expr ic in
			While(p, b)
		| "let" ->
			(* let has an inner tag for binding *)
			let v = read_id ic in
			let t = read_id ic in
			(match read ic with
			| "let_binding_no_init" ->
				let body = read_expr ic in
				Let(v, t, None, body)
			| "let_binding_init" ->
				let init = read_expr ic in
				let body = read_expr ic in
				Let(v, t, Some init, body)
			| x -> failwith ("bad let binding tag: " ^ x))
		| "case" ->
			let scrut = read_expr ic in
			let k = int_of_string (read ic) in
			let rec loop n acc =
				if n = 0 then List.rev acc
				else
					let id = read_id ic in
					let ty = read_id ic in
					let br = read_expr ic in
					loop (n-1) ((id, ty, br) :: acc)
			in
			let branches = loop k [] in
			Case(scrut, branches)
		| "new" ->
			New(read_id ic)
		| "isvoid" ->
			Isvoid(read_expr ic)
		| "plus"   -> Plus(read_expr ic, read_expr ic)
		| "minus"  -> Minus(read_expr ic, read_expr ic)
		| "times"  -> Times(read_expr ic, read_expr ic)
		| "divide" -> Divide(read_expr ic, read_expr ic)
		| "lt"     -> Lt(read_expr ic, read_expr ic)
		| "le"     -> Le(read_expr ic, read_expr ic)
		| "eq"     -> Equals(read_expr ic, read_expr ic)
		| "not"    -> Not(read_expr ic)
		| "negate" -> Tilde(read_expr ic)
		| "identifier" ->
			Identifier(read_id ic)
		| "integer" ->
			Integer(read ic)
		| "string" ->
			String(read ic)
		| "true" -> True
		| "false" -> False
		| x -> failwith ("expression kind unhandled: " ^ x)
	in
	{ loc = eloc; expr_kind = ekind; static_type = None }
