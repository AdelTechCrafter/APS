%{
open Ast
%}
%token <int> NUM
%token <string> IDENT 
%token INT BOOL 
%token TRUE FALSE
%token PLUS MINUS TIMES DIV
%token AND OR EQ LT NOT 
%token IF ECHO CONST FUN REC
%token LPAR RPAR LCRO RCRO
%token COLON SEMICOLON COMA ARROW STAR
/*APS1*/
%token VAR PROC SET IFB WHILE CALL VOID
/*APS2*/
%token LEN NTH ALLOC VEC
%start prog

%type <Ast.prog> prog
%type <Ast.cmds> cmds
%type <Ast.stat> stat
%type <Ast.dec> dec
%type <Ast.tprim> tprim
%type <Ast.types> types
%type <Ast.arg> arg
%type <Ast.args> args
%type <Ast.expr> expr
%type <Ast.exprs> exprs
/*APS1*/
%type <Ast.block> block
/******/
%%

prog:
	LCRO cmds RCRO { ASTProg($2) }
;

/*APS1*/
block:
	LCRO cmds RCRO {ASTBlock($2)}
;

cmds:
	stat {ASTStat($1)}
	| dec SEMICOLON cmds { ASTDec($1,$3) }
	| stat SEMICOLON cmds { ASTStatcmd($1,$3) }
;

stat:
	ECHO expr { ASTEcho($2) }
	/* APS1 */
	|SET lval expr{ASTSet($2,$3)}
	|IFB expr block block {ASTIfb ($2,$3,$4)}
	|WHILE expr block {ASTWhile($2, $3)}
	|CALL expr exprs{ASTCall($2,$3)}

;
/*APS2*/
lval:
	IDENT { ASTLvId($1)}
	|LPAR NTH lval expr RPAR{ASTLval($3,$4)}
;

expr:
	TRUE { ASTTrue }
	| FALSE { ASTFalse }
	| NUM { ASTNum($1) }
	| IDENT { ASTId($1) }
	| LPAR NOT expr RPAR { ASTUPrim(Ast.Not,$3) }
	| LPAR PLUS expr expr RPAR  { ASTBPrim(Ast.Add, $3, $4) }
	| LPAR MINUS expr expr RPAR  { ASTBPrim(Ast.Sub, $3, $4) }
	| LPAR TIMES expr expr RPAR  { ASTBPrim(Ast.Mul, $3, $4) }
	| LPAR DIV expr expr RPAR  { ASTBPrim(Ast.Div, $3, $4) }
	| LPAR AND expr expr RPAR  { ASTBPrim(Ast.And, $3, $4) }
	| LPAR OR expr expr RPAR  { ASTBPrim(Ast.Or, $3, $4) }
	| LPAR EQ expr expr RPAR  { ASTBPrim(Ast.Eq, $3, $4) }
	| LPAR LT expr expr RPAR  { ASTBPrim(Ast.Lt, $3, $4) }
	| LCRO args RCRO expr { ASTLambda($2,$4) }
	| LPAR expr exprs RPAR { ASTApply($2,$3) }
	| LPAR IF expr expr expr RPAR { ASTIf($3,$4,$5) }
	/*APS2*/
	|LPAR LEN expr RPAR{ASTLen($3)}
	|LPAR ALLOC expr RPAR{ASTAlloc($3)}
	|LPAR NTH expr expr RPAR{ASTNth($3,$4)}
;

exprs:
	expr { ASTExpr($1) }
	| expr exprs { ASTExprs($1,$2) }
;
	

dec: 
	CONST IDENT tprim expr { ASTConst($2,$3,$4) }
	| FUN IDENT tprim LCRO args RCRO expr { ASTFun($2,$3,$5,$7) }
	| FUN REC IDENT tprim LCRO args RCRO expr { ASTFunRec($3,$4,$6,$8) }
	/* APS1 */
	|VAR IDENT tprim {ASTVar($2,$3)}
	| PROC IDENT LCRO args RCRO block { ASTProc($2,$4,$6) }
	| PROC REC IDENT LCRO args RCRO block { ASTProcRec($3,$5,$7) }

tprim: 
	INT { Int }
	| BOOL { Bool }
	| LPAR types ARROW tprim RPAR { ASTArrow($2,$4) }
	/*APS2*/
	|LPAR VEC tprim RPAR {ASTVec($3)}
;

types:
	tprim { ASTType($1) }
	| tprim STAR types { ASTTypes($1,$3) }
;

arg:
	IDENT COLON tprim { Arg($1,$3) }
;

args:
	arg {ASTArg($1) }
	| arg COMA args { ASTArgs($1,$3) }
;



