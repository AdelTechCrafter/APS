type token =
  | NUM of (int)
  | IDENT of (string)
  | INT
  | BOOL
  | TRUE of (bool)
  | FALSE of (bool)
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
  | LPAR
  | RPAR
  | LCRO
  | RCRO
  | COLON
  | SEMICOLON
  | COMA
  | ARROW
  | STAR
  | CONST
  | FUN
  | REC
  | EOL

val cmds :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.expr
