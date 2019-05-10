%{
open Ast
%}
%token <int> NUM
%token <string> IDENT TYPEBOOL TYPENUM
%token IF
%token PLUS MINUS TIMES DIV NOT AND OR EQ LT
%token LPAR RPAR LCRO RCRO
%token EOL
%token <bool> BOOL
%token ECHO
%token CONST
%token COLON COMA ARROW STAR SEMICOLON
%token FUN REC


 

%start prog             /* the entry point */
%type <Ast.expr> prog

%%
	prog:
		LCRO EOL cmds EOL RCRO { ASTProg($3) }
	;
	
	cmds:
		stat { $1 }
		| dec SEMICOLON EOL cmds {ASTCmds($1,$4)}
		| stat SEMICOLON EOL cmds {ASTCmds($1,$4)}
	;
	
	mtype:
		TYPENUM {ASTType($1)}
		| TYPEBOOL 	 { ASTType($1) }
		| LPAR mtypes ARROW mtype RPAR { ASTFunType($2,$4) }
	;
	
	mtypes:
		mtype { $1 }
		| mtype STAR mtypes { ASTSequenceType($1,$3) }
	
	
	arg:
		IDENT COLON mtype { ASTArg(ASTId($1),$3) }
	;
	args:
		arg { $1}
		|arg COMA args {ASTListArg($1,$3)}
	;
	
	dec:
		CONST IDENT mtype expr{ ASTDecConst(ASTId($2),$3,$4)}
		| FUN IDENT mtype LCRO args RCRO expr { ASTFun(ASTId($2),$3,$5,$7) }
		| FUN REC IDENT mtype LCRO args RCRO expr { ASTFunRec(ASTId($3),$4,$6,$8) }
	;

	stat:
		ECHO expr{ASTEcho($2)}
	;
	
	expr:
		NUM                        { ASTNum($1) }
		| BOOL 			     		 { ASTBool($1) }
		| IDENT                      { ASTId($1) }
		| LPAR IF expr expr expr RPAR{ASTAlternative($3,$4,$5)}
		| LPAR PLUS expr expr RPAR   { ASTPrim(Ast.Add, $3, $4) }
		| LPAR MINUS expr expr RPAR  { ASTPrim(Ast.Sub, $3, $4) }
		| LPAR TIMES expr expr RPAR  { ASTPrim(Ast.Mul, $3, $4) }
		| LPAR DIV expr expr RPAR    { ASTPrim(Ast.Div, $3, $4) }
		| LPAR AND expr expr RPAR   { ASTLog(Ast.And, $3, $4) }
		| LPAR OR expr expr RPAR  { ASTLog(Ast.Or, $3, $4) }
		| LPAR EQ expr expr RPAR  { ASTLog(Ast.Eq, $3, $4) }
		| LPAR LT expr expr RPAR    { ASTLog(Ast.Lt, $3, $4) }
		| LPAR NOT expr RPAR    { ASTNot(Ast.Not, $3) }
		| LCRO args RCRO expr { ASTFunAno($2,$4) }
		| LPAR expr exprs RPAR { ASTSequence($2,$3)}
		
		;

	exprs:
		expr {$1}
		| expr exprs { ASTSequence($1,$2) }
		;
	
	
	
	
	
	
	
		
		
	
		  
		
