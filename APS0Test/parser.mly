%{
open Ast
%}

%token <int> NUM
%token <string> IDENT 
%token INT BOOL
%token <bool> TRUE FALSE
%token PLUS MINUS TIMES DIV AND OR EQ LT NOT IF ECHO
%token LPAR RPAR LCRO RCRO
%token COLON SEMICOLON COMA ARROW STAR
%token CONST FUN REC
%token EOL

%start prog
%type <Ast.expr> prog

%%

line :
	expr EOL {$1}

prog:
	LCRO EOL cmds EOL RCRO { ASTProg($3) }

cmds:
	stat { $1 }
	| decl SEMICOLON EOL cmds { ASTDecs($1,$4) }
	| stat SEMICOLON EOL cmds { ASTStats($1,$4) }

expr:
	NUM { ASTNum($1) }
	| IDENT { ASTId($1) }
	| TRUE { ASTBool($1) }
	| FALSE { ASTBool($1) }
	| LPAR PLUS expr expr RPAR { ASTPrim(Ast.Add, $3, $4) }
	| LPAR MINUS expr expr RPAR { ASTPrim(Ast.Sub, $3, $4) }
	| LPAR TIMES expr expr RPAR { ASTPrim(Ast.Mul, $3, $4) }
	| LPAR DIV expr expr RPAR { ASTPrim(Ast.Div, $3, $4) }
	| LPAR AND expr expr RPAR { ASTPrim(Ast.And, $3, $4) }
	| LPAR OR expr expr RPAR { ASTPrim(Ast.Or, $3, $4) }
	| LPAR EQ expr expr RPAR { ASTPrim(Ast.Eq, $3, $4) }
	| LPAR LT expr expr RPAR { ASTPrim(Ast.Lt, $3, $4) }
	| LPAR NOT expr RPAR { ASTUnaryPrim(Ast.Not, $3) }
	| LCRO args RCRO expr { ASTAbs($2,$4) }
	| LPAR expr exprs RPAR { ASTApp($2,$3) }
	| LPAR IF expr expr expr RPAR { ASTIf($3,$4,$5) }
;

exprs:
	expr { ASTSingle($1) }
	| expr exprs { ASTSequence($1,$2) }
	

args:
	arg { $1 }
	| arg COMA args { ASTArgs($1,$3) }

arg:
	IDENT COLON tprim { ASTArg(ASTId($1),$3) }
;

tprim: 
	INT { Int }
	| BOOL { Bool }
	| LPAR types ARROW tprim RPAR { TypeFun($2,$4) }
;

types:
	tprim { $1 }
	| tprim STAR types { Types($1,$3) }
;

stat:
	ECHO expr { ASTEcho($2) }
;

decl: 
	CONST IDENT tprim expr { ASTConst($2,$3,$4) }
	| FUN IDENT tprim LCRO args RCRO expr { ASTFun($2,$3,$5,$7) }
	| FUN REC IDENT tprim LCRO args RCRO expr { ASTFunRec($3,$4,$6,$8) }
;


