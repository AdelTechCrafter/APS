{
  open Parser
  exception Eof
}

rule token = parse
(*Séparateurs*)
[' ' '\t' '\n'] { token lexbuf } (* skip blanks *)
(*Constantes numériques*)
| ('-'?)['0'-'9']+ as lxm { NUM(int_of_string lxm) }
(*mots-clefs*)
| "true" { TRUE }
| "false" {FALSE}
| "add" { PLUS }
| "sub" { MINUS }
| "mul" { TIMES }
| "div" { DIV }
| "and" { AND}
| "or" { OR }
| "eq" { EQ }
| "lt" { LT }
| "not" { NOT }
| "int" { INT }
| "bool" { BOOL }
| "if" { IF }
| "ECHO" { ECHO }
| "CONST" { CONST }
| "FUN" { FUN }
| "REC" { REC }
(*Symboles réservés*)
| '(' { LPAR }
| ')' { RPAR }
| '[' { LCRO }
| ']' { RCRO }
| ':' { COLON }
| ';' { SEMICOLON }
| ',' { COMA }
| "->" { ARROW }
| '*' {STAR}
(*end of file*)
| eof { raise Eof }
(*identificateurs*)
| ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']* as id { IDENT(id)}

