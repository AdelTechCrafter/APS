type token =
  | NUM of (int)
  | IDENT of (string)
  | INT
  | BOOL
  | TRUE
  | FALSE
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | AND
  | OR
  | EQ
  | LT
  | NOT
  | IF
  | ECHO
  | CONST
  | FUN
  | REC
  | LPAR
  | RPAR
  | LCRO
  | RCRO
  | COLON
  | SEMICOLON
  | COMA
  | ARROW
  | STAR
  | VAR
  | PROC
  | SET
  | IFB
  | WHILE
  | CALL
  | VOID
  | LEN
  | NTH
  | ALLOC
  | VEC

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.prog
