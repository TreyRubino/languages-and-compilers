
type identifier = string * string
type structure =
  | ClassNoInherits of identifier * (feature list)
  | ClassInherits of identifier * identifier * (feature list)
and feature = 
  | AttributeNoInit of identifier * identifier
  | AttributeInit of identifier * identifier * expr
  | Method of identifier * (formal list) * identifier * expr
and formal = identifier * identifier 
and expr_internal = 
  | Assign of identifier * expr
  | DynamicDispatch of expr * identifier * expr list
  | StaticDispatch of expr * identifier * identifier * expr list 
  | SelfDispatch of identifier * expr list
  | If of expr * expr * expr
  | While of expr * expr
  | Let of (let_binding list) * expr
  | Case of expr * (identifier * identifier * expr) list
  | New of identifier
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
  | Identifier of identifier
  | Integer of string
  | String of string
  | True
  | False
  | Block of expr list
and let_binding =
  | LetBindingNoInit of identifier * identifier
  | LetBindingInit of identifier * identifier * expr
and expr = string * expr_internal

type program = structure list