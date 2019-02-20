type token =
  | NUM of (int)
  | IDENT of (string)
  | TYPEBOOL of (string)
  | TYPENUM of (string)
  | IF
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | NOT
  | AND
  | OR
  | EQ
  | LT
  | LPAR
  | RPAR
  | LCRO
  | RCRO
  | EOL
  | BOOL of (bool)
  | ECHO
  | CONST
  | COLON
  | COMA
  | ARROW
  | STAR
  | SEMICOLON
  | FUN
  | REC

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.expr
