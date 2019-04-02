type op = Add | Mul | Sub | Div | And | Or | Eq | Lt | Not


type tprim = Int | Bool | Types of tprim * tprim 
	| TypeFun of tprim * tprim

type expr =
	ASTNum of int
	| ASTId of string
	| ASTPrim of op * expr * expr
	| ASTUnaryPrim of op * expr
	| ASTBool of bool
	| ASTArg of expr * tprim
	| ASTArgs of expr * expr
	| ASTAbs of expr * expr
	| ASTApp of expr * expr
	| ASTSequence of expr * expr
	| ASTSingle of expr
	| ASTIf of expr * expr * expr
	| ASTEcho of expr
	| ASTConst of string * tprim * expr
	| ASTFun of string * tprim * expr * expr
	| ASTFunRec of string * tprim * expr * expr
	| ASTDecs of expr * expr
	| ASTStats of expr * expr

let string_of_op op = 
	match op with
	 Add -> "add"
	| Mul -> "mul"
	| Sub -> "sub"
	| Div -> "div"
	| And -> "and"
	| Or -> "or"
	| Eq -> "eq"
	| Lt -> "lt"
	| Not -> "not"

let op_of_string op =
	match op with
	"add" -> Add
	| "mul" -> Mul
	| "sub" -> Sub
	| "div" -> Div
	| "and" -> And
	| "or" -> Or
	| "eq" -> Eq
	| "lt" -> Lt
	| "not" -> Not
	| _ -> failwith "not an operator"

let rec string_of_type t =
	match t with
	Int -> "int"
	| Bool -> "bool"
	| Types(t1,t2) -> (string_of_type t1)^","^(string_of_type t2)^","
	| TypeFun(t1,t2) -> "(["^(string_of_type t1)^"],"^(string_of_type t2)^")" 






