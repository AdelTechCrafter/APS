type opbin = Add | Mul | Sub | Div | And | Or | Eq | Lt 

and opun=Not


and tprim = 
	Int 
	|Bool
	(*APS1*)
	|Void
	|ASTArrow of types * tprim

and types= 
	ASTType of tprim
	|ASTTypes of tprim * types
	
and arg=
	ASTArg of string * tprim

and args= 
	ASTArg of arg
	|ASTArgs of arg * args
	

and expr =
	ASTTrue
	| ASTFalse
	| ASTNum of int
	| ASTId of string
	| ASTBPrim of opbin * expr * expr
	| ASTUPrim of opun * expr
	| ASTIf of expr * expr * expr
	| ASTLambda of args * expr
	| ASTApply of expr * exprs
	
and exprs=
	ASTExpr of expr
	|ASTExprs of expr * exprs

and dec=
	| ASTConst of string * tprim * expr
	| ASTFun of string * tprim * args * expr
	| ASTFunRec of string * tprim * args * expr
	(*APS1*)
	|ASTVar of string * tprim
	| ASTProc of string * args * block
	| ASTProcRec of string * args * block
	

and stat=
	ASTEcho of expr
	(*APS1*)
	|ASTSet of string * expr
	|ASTIfb of expr * block * block
	|ASTWhile of expr * block
	|ASTCall of expr * exprs

and cmds=
	ASTStat of stat
	|ASTDec of dec * cmds
	|ASTStatcmd of stat * cmds

(*APS1*)
and block=
	ASTBlock of cmds

and prog=
	ASTProg of cmds

let string_of_opbin opbin = 
	match opbin with
	 Add -> "add"
	| Mul -> "mul"
	| Sub -> "sub"
	| Div -> "div"
	| And -> "and"
	| Or -> "or"
	| Eq -> "eq"
	| Lt -> "lt"

let opbin_of_string opbin =
	match opbin with
	"add" -> Add
	| "mul" -> Mul
	| "sub" -> Sub
	| "div" -> Div
	| "and" -> And
	| "or" -> Or
	| "eq" -> Eq
	| "lt" -> Lt
	| _ -> failwith "not an operator"

let string_of_opun opun = 
	match opun with
		|Not -> "not"

let opun_of_string opun =
	match opun with
	| "not" -> Not
	| _ -> failwith "not an operator"	
