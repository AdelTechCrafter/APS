%{
open Ast
%}
%token <int> NUM
%token <string> IDENT
%token IF
%token PLUS MINUS TIMES DIV
%token LPAR RPAR LCRO RCRO
%token EOL
%token <bool> BOOL

 

%start line             /* the entry point */
%type <Ast.expr> line

%%
	line:
	expr EOL                     { $1 }
	;
	
	
	expr:
		NUM                        { ASTNum($1) }
	| BOOL 			     		 { ASTBool($1) }
	| IDENT                      { ASTId($1) }
	| LPAR PLUS expr expr RPAR   { ASTPrim(Ast.Add, $3, $4) }
	| LPAR MINUS expr expr RPAR  { ASTPrim(Ast.Sub, $3, $4) }
	| LPAR TIMES expr expr RPAR  { ASTPrim(Ast.Mul, $3, $4) }
	| LPAR DIV expr expr RPAR    { ASTPrim(Ast.Div, $3, $4) }
	(*| LCRO args RCRO expr{}*)
	(*| LPAR expr exprs RPAR{$1;$2}*)
	| LPAR IF expr expr expr RPAR{ASTAlternative($3,$4,$5)}
	;
	
	
	(* prog:
		LCRO cmds RCRO
	cmds:
		stat
		|dec ';' cmds
		|stat ';' cmds*)
		
	(*
	exprs:
		expr {$1}
		|expr exprs{$2;ASTSequence($3)};*)
	
	
	(*arg:
		IDENT ':' mtype;
	args:
		arg
		|arg ',' args;
	mtype:
		NUM
		|BOOL
	mtypes:
		mtype
		|mtype '*' mtypes;
	
	
	dec:
		IDENT mtype expr
		|"CONST" IDENT mtype expr
		|"FUN" IDENT mtype LCRO args RCRO expr
		|"FUN REC" IDENT mtype LCRO args RCRO expr;*)
	stat:
		"ECHO" expr{ASTEcho($2)}
	
