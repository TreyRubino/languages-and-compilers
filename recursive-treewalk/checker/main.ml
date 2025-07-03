(* Trey Rubino -- COOL Checker -- Dr. Schwesinger *)

open Printf

exception TYPE_ERROR

(* we need a notation of a static type *)

type static_type =
    | Class of string
    | SELF_TYPE of string

let type_to_str t = match t with
    | Class(x) -> x
    | SELF_TYPE(c) -> "SELF_TYPE"

(* <= subtyping (liskov substrituion principle) *)
let rec is_subtype t1 t2 =
    match t1, t2 with
    | Class(x), Class(y) when x = y -> true
    | Class(x), Class("Object") -> true
    | Class(x), Class(y) -> false (* check the parent map *)
    | _, _ -> false 

let is_primitive t = 
    match t with 
    | Class "Int" | Class "String" | Class "Bool" -> true
    | _ -> false
in 

(* mapping from object identifiers to type *)
type object_env = 
    (string, static_type) Hashtbl.t 

let empty_object_env () = Hashtbl.create 255

(* Cool AST *)
type cool_program = cool_class list
and loc = string
and id = loc * string
and cool_type = id
and cool_class = id * (id option) * feature list
and feature = 
    | Attribute of id * cool_type * (expr option)
    | Method  of id * (formal list) * cool_type * expr
and formal = id * cool_type
(*  We want to leave room to annotate the expression with its static type
 *   We dont't know the type at the start but we will discover it by typechecking. 
 *  So it will be Mutable. *)
and expr = {
    loc : loc ;
    expr_kind : expr_kind ; 
    mutable static_type : static_type option ; 
}
and expr_kind = 
    | Assign of id * expr
    | DynamicDispatch of expr * id * expr list
    | StaticDispatch of expr * id * id * expr list 
    | SelfDispatch of id * expr list
    | Let of id * id * (expr option) * expr (* let _x_ : _t_ <- _e1_ in _e2_ *)
    | If of expr * expr * expr
    | While of expr * expr
    | New of id
    | Isvoid of expr
    | Plus of expr * expr
    | Minus of expr * expr
    | Times of expr * expr
    | Divide of expr * expr
    | Tilde of expr
    | Lt of expr * expr
    | Le of expr * expr
    | Equals of expr * expr
    | Not of expr
    | Identifier of id
    | Integer of string
    | String of string
    | True
    | False

let main () = 
begin
    let fname = Sys.argv.(1) in
    let fin = open_in fname in

    let read () = input_line fin in

    let rec range k = if k <= 0 then [] else k :: (range (k - 1)) in

    let read_list worker = 
        let k = int_of_string (read ()) in 
        let lst = range k in
        List.map (fun _ -> worker ()) lst
    in

    let rec read_cool_program () = 
        read_list read_cool_class
    and read_id () =
        let loc = read () in
        let name = read () in 
        (loc, name) 
    and read_cool_class () = 
        let cname = read_id () in
        let inherits = 
            match read () with
            | "no_inherits" -> None
            | "inherits" -> 
                let super = read_id () in
                Some(super)
        in
        let features = read_list read_feature in
        (cname, inherits, features)
    and read_feature () =
        match read () with
        | "attribute_no_init" ->
            let fname = read_id () in
            let ftype = read_id () in
            Attribute(fname, ftype, None)
        | "attribute_init" -> 
            let fname = read_id () in
            let ftype = read_id () in
            let finit = read_expr () in
            Attribute(fname, ftype, Some(finit))
        | "method" ->
            let mname = read_id () in
            let formals = read_list read_formal in
            let mtype = read_id () in
            let mbody = read_expr () in
            Method(mname, formals, mtype, mbody)
    and read_formal () = 
        let fname = read_id () in 
        let ftype = read_id () in
        (fname, ftype)
    and read_expr () = 
        let eloc = read () in 
        let ekind =  
            match read () with 
            | "integer" -> 
                let ival = read () in
                Integer(ival)
            | "plus" -> 
                let expr1 = read_expr () in 
                let expr2 = read_expr () in 
                Plus(expr1, expr2)
            | "indentifier" -> 
                let id = read_id () in
                Identifier(id)
            | "let" -> (* only consider one let binding *)
                let num_bindings = read () in 
                let no_init = read () in 
                let let_var = read_id () in
                let let_type = read_id () in 
                let let_body = read_expr () in 
                Let (let_var, let_type, None, let_body)
            | x -> failwith ("expression kind unhandled: " ^ x)
        in
        { 
            loc = eloc ; 
            expr_kind = ekind ;
            static_type = None;
        }
    in
        
    let ast = read_cool_program () in
    close_in fin ; 

    (* check for class related errors *)

    let base_classes = [ "Int" ; "String" ; "Bool" ; "IO" ; "Object" ] in
    let user_classes = List.map (fun ((_, cname), _, _) -> cname) ast in
    let all_classes = List.sort compare (base_classes @ user_classes) in 

    List.iter (fun ((cloc, cname), inherits, features) -> 
        match inherits with
        | None -> ()
        | Some(iloc, iname) ->
            if iname = "Int" then (
                printf "ERROR: %s: Type-Check: Inheriting from forbidden class %s\n" iloc iname ; exit 1
            ) ;
            if not (List.mem iname all_classes) then (
                printf "ERROR: %s: Type-Check: Inheriting from undefined class %s\n" iloc iname ; exit 1
            ) ; 
    ) ast ; 

    (* This is the time to do expression typechecking. this is the heart of
     *   of this assignment 
     *  We want to iterate over every class.
     *  Then over every feature
     *  Then type check the expressions in that feature.
     *)

    let rec type_check (o : object_env) (expr : expr) : static_type = 
        let check exprs expected = 
            List.iter (fun expr -> 
                match type_check o expr with
                | Class t when t = expected -> ()
                | Class t -> raise TYPE_ERROR
                | _ -> raise TYPE_ERROR 
            ) exprs ; 
            Class expected
        in 

        let static_type = 
            match expr.expr_kind with
            | Plus(expr1, expr2) -> 
                check [expr1 ; expr2] "Int"
            | Minus(expr1, expr2) -> 
                check [expr1 ; expr2] "Int"
            | Times(expr1, expr2) -> 
                check [expr1 ; expr2] "Int"
            | Divide(expr1, expr2) -> 
                check [expr1 ; expr2] "Int"
            | Tilde(expr) -> 
                check [expr] "Bool"
            | Lt(expr1, expr2) -> 
                check [expr1 ; expr2] "Bool"
            | Le(expr1, expr2) -> 
                check [expr1 ; expr2] "Bool"
            | Lt(expr1, expr2) -> 
                check [expr1 ; expr2] "Bool"    
            | Equals(expr, expr) ->
                let type1 = type_check o expr1 in
                let type2 = type_check o expr2 in   

                if is_primitive type1 || is_primitive type2 then (
                    if type1 <> type2 then raise TYPE_ERROR 
                ) ; 

                (Class "Bool")
            | Not(expr) -> 
                check expr "Bool"
            | Let((vloc, vname), (type_loc, type_name), None, let_body) -> 
                Hashtbl.add o vname (Class type_name) ;
                let body_type = type_check o let_body in
                Hashtbl.remove o vname ; 
                body_type
            | Assign((vloc, vname), expr) -> 
                if Hashtbl.mem o vname then (
                    let declared_type = Hashtbl.find o vname in
                    let expr_type = type_check o expr in

                    if is_subtype expr_type declared_type then
                        expr_type
                    else (
                        raise TYPE_ERROR
                    )
                ) else ( 
                    Printf.printf "ERROR: %s: Type-Check: Undeclared variable %s\n" vloc vname ; 
                    exit 1
                )
            | Identifier((vloc, vname)) ->
                if Hashtbl.mem o vname then (
                    Hashtbl.find o vname
                ) else ( 
                    printf "ERROR: %s: Type-Check: Undeclared variable %s\n" vloc vname ; exit 1
                ) ; 
            | Integer(i) -> (Class "Int") 
            | String(s) -> (Class "String")
            | True -> (Class "Bool")
            | False -> (Class "Bool")
        in
        expr.static_type <- Some(static_type) ; 
        static_type 
    in

    List.iter (fun ((cloc, cname), inherits, features) -> 
        List.iter (fun feat -> 
            match feat with
            | Attribute((nameloc, name), (dtloc, declared_type), Some(init)) ->
                let o = empty_object_env () in
                let init_type = type_check o init in 
                if (is_subtype init_type) (Class declared_type) then (
                    ()
                ) else (
                    printf "ERROR: %s: Type-Check: Initialzier for %s for %s not %s\n" nameloc name (type_to_str init_type) declared_type ; exit 1
                ) ; 
            | _ -> ()
        ) features ;
    ) ast ;

    (* now start to build the class map and output file *)
    (* this is where we recursively walk the ast and perform *) 
    (* various semantic validations *)
    let cmname = Filename.chop_extension fname ^ ".cl-type" in 
    let fout = open_out cmname in

    let rec output_expr e = 
        fprintf fout "%s\n" e.loc ;
        (match e.static_type with
        | None -> failwith "we forgot to do typechecking."
        | Some(Class(c)) -> fprintf fout "%s\n" c
        | Some(SELF_TYPE(c)) -> failwith "fix this."
        ) ;  
        match e.expr_kind with
        | Integer(ival) -> fprintf fout "integer\n%s\n" ival
    in

    (* recursively tree walk *)
    fprintf fout "class_map\n%d\n" (List.length all_classes) ; 
    List.iter (fun cname -> 
        fprintf fout "%s\n" cname ;
        
        let attributes = 
            try
                let _, inherits, features = List.find (fun ((_, cname2), _, _) -> cname = cname2) ast in 
                List.filter (fun feature -> 
                    match feature with
                    | Attribute _ -> true
                    | Method _ -> false 
                ) features 
            with Not_found -> [] (* bool/int/object do not have attributes *) 
        in 

        fprintf fout "%d\n" (List.length attributes) ; 
        List.iter (fun attr -> 
            match attr with 
            | Attribute((_, aname), (_, atype), None) -> 
            fprintf fout "no_initializer\n%s\n%s\n" aname atype
            | Attribute((_, aname), (_, atype), (Some init)) ->
            fprintf fout "initializer\n%s\n%s\n" aname atype ;
            output_expr init
            | Method _ -> failwith "Method unexpected" 
        ) attributes ;
    ) all_classes ; 
    close_out fout ;
end ;;
main () ;;