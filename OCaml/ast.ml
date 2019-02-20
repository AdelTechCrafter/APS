type op = Add | Mul | Sub | Div | Not | And | Or | Eq | Lt 
type expr =
		ASTNum of int
	| ASTId of string
	| ASTPrim of op * expr * expr
	| ASTBool of bool
	| ASTAlternative of expr * expr * expr
	| ASTEcho of expr
	| ASTDec of expr * expr * expr
	| ASTDecConst of expr * expr * expr
	| ASTType of string
	| ASTArg of expr * expr
	| ASTSequenceType of expr * expr
	| ASTFunType of expr * expr
	| ASTListArg of expr * expr
	| ASTFunAno of expr *expr
	| ASTSequence of expr * expr
	| ASTFun of expr * expr * expr * expr
	| ASTFunRec of expr * expr * expr * expr
	| ASTLog of op * expr * expr
	| ASTNot of op * expr
	| ASTCmds of expr * expr
	| ASTProg of expr
	

let string_of_op op =
	match op with
		Add -> "add"
		| Mul -> "mul"
		| Sub -> "sub"
		| Div -> "div"
		| Not -> "not"
		| And -> "and"
		| Or -> "or"
		| Eq -> "eq"
		| Lt -> "lt"
		
	
let op_of_string op =
	match op with
		"add" -> Add
		| "mul" -> Mul
		| "sub" -> Sub
		| "div" -> Div
		| "not" -> Not
		| "and" -> And
		| "or" -> Or
		| "eq" -> Eq
		| "lt" -> Lt
		| _-> failwith "Error op matching"

