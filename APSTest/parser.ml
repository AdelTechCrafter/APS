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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Ast
# 38 "parser.ml"
let yytransl_const = [|
  259 (* INT *);
  260 (* BOOL *);
  263 (* PLUS *);
  264 (* MINUS *);
  265 (* TIMES *);
  266 (* DIV *);
  267 (* AND *);
  268 (* OR *);
  269 (* EQ *);
  270 (* LT *);
  271 (* NOT *);
  272 (* IF *);
  273 (* ECHO *);
  274 (* LPAR *);
  275 (* RPAR *);
  276 (* LCRO *);
  277 (* RCRO *);
  278 (* COLON *);
  279 (* SEMICOLON *);
  280 (* COMA *);
  281 (* ARROW *);
  282 (* STAR *);
  283 (* CONST *);
  284 (* FUN *);
  285 (* REC *);
  286 (* EOL *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
  261 (* TRUE *);
  262 (* FALSE *);
    0|]

let yylhs = "\255\255\
\002\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\005\000\005\000\004\000\004\000\006\000\007\000\007\000\
\007\000\008\000\008\000\009\000\010\000\010\000\010\000\001\000\
\001\000\001\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\001\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\004\000\004\000\004\000\
\006\000\001\000\002\000\001\000\003\000\003\000\001\000\001\000\
\005\000\001\000\003\000\002\000\004\000\007\000\008\000\001\000\
\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\035\000\000\000\000\000\
\002\000\003\000\004\000\005\000\000\000\000\000\028\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\023\000\024\000\000\000\000\000\000\000\000\000\
\034\000\033\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\029\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\014\000\000\000\
\019\000\016\000\022\000\015\000\021\000\000\000\000\000\000\000\
\000\000\006\000\007\000\008\000\009\000\010\000\011\000\012\000\
\013\000\000\000\027\000\000\000\000\000\000\000\017\000\025\000\
\030\000\000\000\031\000"

let yydgoto = "\002\000\
\006\000\000\000\053\000\033\000\054\000\034\000\058\000\059\000\
\007\000\008\000"

let yysindex = "\003\000\
\031\255\000\000\032\255\004\255\255\254\000\000\240\254\241\254\
\000\000\000\000\000\000\000\000\071\255\009\255\000\000\012\255\
\012\255\015\255\031\255\031\255\032\255\032\255\032\255\032\255\
\032\255\032\255\032\255\032\255\032\255\032\255\032\255\248\254\
\010\255\008\255\000\000\000\000\012\255\032\255\019\255\012\255\
\000\000\000\000\032\255\032\255\032\255\032\255\032\255\032\255\
\032\255\032\255\035\255\032\255\032\255\036\255\012\255\032\255\
\009\255\030\255\037\255\000\000\009\255\040\255\038\255\042\255\
\044\255\045\255\046\255\047\255\048\255\049\255\000\000\032\255\
\000\000\000\000\000\000\000\000\000\000\012\255\012\255\050\255\
\009\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\051\255\000\000\055\255\032\255\067\255\000\000\000\000\
\000\000\032\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\092\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\072\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\075\255\000\000\000\000\000\000\
\000\000\073\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\239\255\000\000\253\255\204\255\043\000\000\000\252\255\019\000\
\000\000\000\000"

let yytablesize = 98
let yytable = "\015\000\
\017\000\041\000\042\000\001\000\077\000\016\000\019\000\020\000\
\080\000\031\000\032\000\038\000\039\000\055\000\035\000\036\000\
\040\000\043\000\044\000\045\000\046\000\047\000\048\000\049\000\
\050\000\051\000\052\000\018\000\094\000\037\000\056\000\057\000\
\009\000\010\000\060\000\062\000\011\000\012\000\061\000\063\000\
\064\000\065\000\066\000\067\000\068\000\069\000\070\000\003\000\
\072\000\013\000\075\000\014\000\076\000\071\000\074\000\078\000\
\082\000\004\000\005\000\081\000\083\000\079\000\084\000\085\000\
\086\000\087\000\088\000\089\000\090\000\095\000\093\000\009\000\
\010\000\096\000\092\000\011\000\012\000\021\000\022\000\023\000\
\024\000\025\000\026\000\027\000\028\000\029\000\030\000\098\000\
\013\000\097\000\014\000\032\000\020\000\018\000\099\000\073\000\
\091\000\026\000"

let yycheck = "\003\000\
\002\001\019\000\020\000\001\000\057\000\002\001\023\001\023\001\
\061\000\013\000\002\001\016\000\017\000\022\001\003\001\004\001\
\002\001\021\000\022\000\023\000\024\000\025\000\026\000\027\000\
\028\000\029\000\030\000\029\001\081\000\018\001\021\001\024\001\
\001\001\002\001\038\000\040\000\005\001\006\001\020\001\043\000\
\044\000\045\000\046\000\047\000\048\000\049\000\050\000\017\001\
\052\000\018\001\055\000\020\001\056\000\019\001\019\001\026\001\
\019\001\027\001\028\001\020\001\019\001\025\001\019\001\019\001\
\019\001\019\001\019\001\019\001\072\000\019\001\021\001\001\001\
\002\001\019\001\079\000\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\021\001\
\018\001\093\000\020\001\000\000\021\001\019\001\098\000\053\000\
\078\000\025\001"

let yynames_const = "\
  INT\000\
  BOOL\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  AND\000\
  OR\000\
  EQ\000\
  LT\000\
  NOT\000\
  IF\000\
  ECHO\000\
  LPAR\000\
  RPAR\000\
  LCRO\000\
  RCRO\000\
  COLON\000\
  SEMICOLON\000\
  COMA\000\
  ARROW\000\
  STAR\000\
  CONST\000\
  FUN\000\
  REC\000\
  EOL\000\
  "

let yynames_block = "\
  NUM\000\
  IDENT\000\
  TRUE\000\
  FALSE\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 22 "parser.mly"
          ( _1 )
# 216 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 26 "parser.mly"
     ( ASTNum(_1) )
# 223 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 27 "parser.mly"
         ( ASTId(_1) )
# 230 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 28 "parser.mly"
        ( ASTBool(_1) )
# 237 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 29 "parser.mly"
         ( ASTBool(_1) )
# 244 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 30 "parser.mly"
                            ( ASTPrim(Ast.Add, _3, _4) )
# 252 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 31 "parser.mly"
                             ( ASTPrim(Ast.Sub, _3, _4) )
# 260 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 32 "parser.mly"
                             ( ASTPrim(Ast.Mul, _3, _4) )
# 268 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 33 "parser.mly"
                           ( ASTPrim(Ast.Div, _3, _4) )
# 276 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 34 "parser.mly"
                           ( ASTPrim(Ast.And, _3, _4) )
# 284 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 35 "parser.mly"
                          ( ASTPrim(Ast.Or, _3, _4) )
# 292 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 36 "parser.mly"
                          ( ASTPrim(Ast.Eq, _3, _4) )
# 300 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 37 "parser.mly"
                          ( ASTPrim(Ast.Lt, _3, _4) )
# 308 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 38 "parser.mly"
                      ( ASTUnaryPrim(Ast.Not, _3) )
# 315 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 39 "parser.mly"
                       ( ASTAbs(_2,_4) )
# 323 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 40 "parser.mly"
                        ( ASTApp(_2,_3) )
# 331 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 41 "parser.mly"
                               ( ASTIf(_3,_4,_5) )
# 340 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 45 "parser.mly"
      ( ASTSingle(_1) )
# 347 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 46 "parser.mly"
              ( ASTSequence(_1,_2) )
# 355 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 50 "parser.mly"
     ( _1 )
# 362 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 51 "parser.mly"
                 ( ASTArgs(_1,_3) )
# 370 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tprim) in
    Obj.repr(
# 54 "parser.mly"
                   ( ASTArg(ASTId(_1),_3) )
# 378 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "parser.mly"
     ( Int )
# 384 "parser.ml"
               : 'tprim))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
        ( Bool )
# 390 "parser.ml"
               : 'tprim))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'types) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'tprim) in
    Obj.repr(
# 60 "parser.mly"
                               ( TypeFun(_2,_4) )
# 398 "parser.ml"
               : 'tprim))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tprim) in
    Obj.repr(
# 64 "parser.mly"
       ( _1 )
# 405 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'tprim) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'types) in
    Obj.repr(
# 65 "parser.mly"
                    ( Types(_1,_3) )
# 413 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 69 "parser.mly"
           ( ASTEcho(_2) )
# 420 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'tprim) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 73 "parser.mly"
                        ( ASTConst(_2,_3,_4) )
# 429 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'tprim) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 74 "parser.mly"
                                       ( ASTFun(_2,_3,_5,_7) )
# 439 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'tprim) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 75 "parser.mly"
                                           ( ASTFunRec(_3,_4,_6,_8) )
# 449 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 79 "parser.mly"
      ( _1 )
# 456 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'decl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 80 "parser.mly"
                       ( ASTDecs(_1,_3) )
# 464 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 81 "parser.mly"
                       ( ASTStats(_1,_3) )
# 472 "parser.ml"
               : Ast.expr))
(* Entry cmds *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let cmds (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.expr)
