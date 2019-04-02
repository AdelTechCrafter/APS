{
  open Parser
  exception Eof
}

rule token = parse
[' ' '\t'] { token lexbuf } (* skip blanks *)
| ['\n' ] { EOL }
| ['0'-'9']+('.'['0'-'9'])? as lxm { NUM(int_of_string lxm) }
| "true" { TRUE(true) }
| "false" {FALSE(false)}
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
| ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']* as id { IDENT(id)}
| '(' { LPAR }
| ')' { RPAR }
| '[' { LCRO }
| ']' { RCRO }
| ':' { COLON }
| ';' { SEMICOLON }
| ',' { COMA }
| "->" { ARROW }
| '*' {STAR}
| eof { raise Eof }

