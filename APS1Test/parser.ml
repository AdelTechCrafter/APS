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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Ast
# 44 "parser.ml"
let yytransl_const = [|
  259 (* INT *);
  260 (* BOOL *);
  261 (* TRUE *);
  262 (* FALSE *);
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
  274 (* CONST *);
  275 (* FUN *);
  276 (* REC *);
  277 (* LPAR *);
  278 (* RPAR *);
  279 (* LCRO *);
  280 (* RCRO *);
  281 (* COLON *);
  282 (* SEMICOLON *);
  283 (* COMA *);
  284 (* ARROW *);
  285 (* STAR *);
  286 (* VAR *);
  287 (* PROC *);
  288 (* SET *);
  289 (* IFB *);
  290 (* WHILE *);
  291 (* CALL *);
  292 (* VOID *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\011\000\002\000\002\000\002\000\003\000\003\000\003\000\
\003\000\003\000\004\000\004\000\004\000\004\000\004\000\004\000\
\005\000\005\000\005\000\006\000\006\000\007\000\008\000\008\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\010\000\010\000\000\000"

let yylen = "\002\000\
\003\000\003\000\001\000\003\000\003\000\002\000\003\000\004\000\
\003\000\003\000\004\000\007\000\008\000\003\000\006\000\007\000\
\001\000\001\000\005\000\001\000\003\000\003\000\001\000\003\000\
\001\000\001\000\001\000\001\000\004\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\004\000\004\000\006\000\
\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\043\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\027\000\028\000\025\000\026\000\000\000\000\000\006\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\017\000\018\000\000\000\000\000\000\000\000\000\
\014\000\000\000\000\000\007\000\000\000\000\000\009\000\000\000\
\010\000\005\000\004\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\011\000\000\000\000\000\000\000\000\000\
\000\000\008\000\042\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\029\000\000\000\039\000\022\000\024\000\
\038\000\000\000\000\000\000\000\000\000\000\000\000\000\002\000\
\030\000\031\000\032\000\033\000\034\000\035\000\036\000\037\000\
\000\000\021\000\000\000\000\000\000\000\015\000\000\000\040\000\
\019\000\012\000\000\000\016\000\013\000"

let yydgoto = "\002\000\
\004\000\014\000\015\000\016\000\082\000\083\000\049\000\050\000\
\064\000\065\000\062\000"

let yysindex = "\013\000\
\250\254\000\000\080\255\000\000\054\255\022\255\000\255\028\255\
\024\255\041\255\054\255\054\255\054\255\021\255\020\255\025\255\
\000\000\000\000\000\000\000\000\122\255\045\255\000\000\006\255\
\006\255\046\255\006\255\030\255\052\255\054\255\034\255\034\255\
\054\255\000\000\080\255\080\255\054\255\054\255\054\255\054\255\
\054\255\054\255\054\255\054\255\054\255\054\255\054\255\033\255\
\035\255\037\255\000\000\000\000\006\255\054\255\048\255\006\255\
\000\000\045\255\051\255\000\000\080\255\034\255\000\000\054\255\
\000\000\000\000\000\000\054\255\054\255\054\255\054\255\054\255\
\054\255\054\255\054\255\056\255\054\255\057\255\006\255\045\255\
\054\255\053\255\055\255\000\000\045\255\058\255\060\255\045\255\
\061\255\000\000\000\000\064\255\065\255\066\255\067\255\069\255\
\070\255\071\255\072\255\000\000\054\255\000\000\000\000\000\000\
\000\000\006\255\006\255\076\255\045\255\034\255\078\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\073\255\000\000\081\255\054\255\082\255\000\000\034\255\000\000\
\000\000\000\000\054\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\084\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\085\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\245\254\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\077\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\224\255\000\000\000\000\250\255\230\255\000\000\199\255\
\251\255\214\255\236\255"

let yytablesize = 145
let yytable = "\023\000\
\087\000\025\000\066\000\067\000\078\000\031\000\032\000\033\000\
\051\000\052\000\041\000\063\000\041\000\001\000\041\000\047\000\
\003\000\054\000\055\000\026\000\057\000\091\000\104\000\024\000\
\060\000\028\000\053\000\108\000\089\000\027\000\111\000\068\000\
\069\000\070\000\071\000\072\000\073\000\074\000\075\000\076\000\
\077\000\090\000\030\000\029\000\034\000\035\000\048\000\056\000\
\084\000\086\000\036\000\125\000\058\000\059\000\017\000\018\000\
\061\000\079\000\019\000\020\000\081\000\080\000\092\000\093\000\
\094\000\095\000\096\000\097\000\098\000\099\000\085\000\101\000\
\103\000\088\000\021\000\105\000\022\000\100\000\102\000\122\000\
\109\000\106\000\107\000\110\000\112\000\113\000\114\000\115\000\
\116\000\126\000\117\000\118\000\119\000\120\000\128\000\121\000\
\005\000\006\000\007\000\124\000\123\000\127\000\129\000\000\000\
\020\000\131\000\132\000\003\000\023\000\008\000\009\000\010\000\
\011\000\012\000\013\000\000\000\000\000\000\000\130\000\000\000\
\000\000\000\000\017\000\018\000\000\000\133\000\019\000\020\000\
\037\000\038\000\039\000\040\000\041\000\042\000\043\000\044\000\
\045\000\046\000\000\000\000\000\000\000\000\000\021\000\000\000\
\022\000"

let yycheck = "\005\000\
\058\000\002\001\035\000\036\000\047\000\011\000\012\000\013\000\
\003\001\004\001\022\001\032\000\024\001\001\000\026\001\021\000\
\023\001\024\000\025\000\020\001\027\000\064\000\080\000\002\001\
\030\000\002\001\021\001\085\000\061\000\002\001\088\000\037\000\
\038\000\039\000\040\000\041\000\042\000\043\000\044\000\045\000\
\046\000\062\000\002\001\020\001\024\001\026\001\002\001\002\001\
\054\000\056\000\026\001\109\000\023\001\002\001\001\001\002\001\
\023\001\025\001\005\001\006\001\024\001\027\001\068\000\069\000\
\070\000\071\000\072\000\073\000\074\000\075\000\023\001\077\000\
\079\000\023\001\021\001\081\000\023\001\022\001\022\001\106\000\
\023\001\029\001\028\001\024\001\024\001\022\001\022\001\022\001\
\022\001\110\000\022\001\022\001\022\001\022\001\022\001\101\000\
\017\001\018\001\019\001\024\001\107\000\024\001\022\001\255\255\
\028\001\024\001\127\000\024\001\024\001\030\001\031\001\032\001\
\033\001\034\001\035\001\255\255\255\255\255\255\124\000\255\255\
\255\255\255\255\001\001\002\001\255\255\131\000\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\013\001\014\001\
\015\001\016\001\255\255\255\255\255\255\255\255\021\001\255\255\
\023\001"

let yynames_const = "\
  INT\000\
  BOOL\000\
  TRUE\000\
  FALSE\000\
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
  CONST\000\
  FUN\000\
  REC\000\
  LPAR\000\
  RPAR\000\
  LCRO\000\
  RCRO\000\
  COLON\000\
  SEMICOLON\000\
  COMA\000\
  ARROW\000\
  STAR\000\
  VAR\000\
  PROC\000\
  SET\000\
  IFB\000\
  WHILE\000\
  CALL\000\
  VOID\000\
  "

let yynames_block = "\
  NUM\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmds) in
    Obj.repr(
# 33 "parser.mly"
                ( ASTProg(_2) )
# 260 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmds) in
    Obj.repr(
# 38 "parser.mly"
                (ASTBlock(_2))
# 267 "parser.ml"
               : Ast.block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stat) in
    Obj.repr(
# 42 "parser.mly"
      (ASTStat(_1))
# 274 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.dec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 43 "parser.mly"
                      ( ASTDec(_1,_3) )
# 282 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 44 "parser.mly"
                       ( ASTStatcmd(_1,_3) )
# 290 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 48 "parser.mly"
           ( ASTEcho(_2) )
# 297 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 50 "parser.mly"
                (ASTSet(_2,_3))
# 305 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.block) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 51 "parser.mly"
                       (ASTIfb (_2,_3,_4))
# 314 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 52 "parser.mly"
                   (ASTWhile(_2, _3))
# 322 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exprs) in
    Obj.repr(
# 53 "parser.mly"
                 (ASTCall(_2,_3))
# 330 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.tprim) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 57 "parser.mly"
                        ( ASTConst(_2,_3,_4) )
# 339 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.tprim) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 58 "parser.mly"
                                       ( ASTFun(_2,_3,_5,_7) )
# 349 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.tprim) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 59 "parser.mly"
                                           ( ASTFunRec(_3,_4,_6,_8) )
# 359 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.tprim) in
    Obj.repr(
# 61 "parser.mly"
                  (ASTVar(_2,_3))
# 367 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 62 "parser.mly"
                                   ( ASTProc(_2,_4,_6) )
# 376 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 63 "parser.mly"
                                       ( ASTProcRec(_3,_5,_7) )
# 385 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "parser.mly"
     ( Int )
# 391 "parser.ml"
               : Ast.tprim))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "parser.mly"
        ( Bool )
# 397 "parser.ml"
               : Ast.tprim))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Ast.types) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.tprim) in
    Obj.repr(
# 68 "parser.mly"
                               ( ASTArrow(_2,_4) )
# 405 "parser.ml"
               : Ast.tprim))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.tprim) in
    Obj.repr(
# 72 "parser.mly"
       ( ASTType(_1) )
# 412 "parser.ml"
               : Ast.types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.tprim) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.types) in
    Obj.repr(
# 73 "parser.mly"
                    ( ASTTypes(_1,_3) )
# 420 "parser.ml"
               : Ast.types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.tprim) in
    Obj.repr(
# 77 "parser.mly"
                   ( Arg(_1,_3) )
# 428 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg) in
    Obj.repr(
# 81 "parser.mly"
     (ASTArg(_1) )
# 435 "parser.ml"
               : Ast.args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.args) in
    Obj.repr(
# 82 "parser.mly"
                 ( ASTArgs(_1,_3) )
# 443 "parser.ml"
               : Ast.args))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
      ( ASTTrue )
# 449 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
         ( ASTFalse )
# 455 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 88 "parser.mly"
       ( ASTNum(_1) )
# 462 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 89 "parser.mly"
         ( ASTId(_1) )
# 469 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 90 "parser.mly"
                      ( ASTUPrim(Ast.Not,_3) )
# 476 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 91 "parser.mly"
                             ( ASTBPrim(Ast.Add, _3, _4) )
# 484 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 92 "parser.mly"
                              ( ASTBPrim(Ast.Sub, _3, _4) )
# 492 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 93 "parser.mly"
                              ( ASTBPrim(Ast.Mul, _3, _4) )
# 500 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 94 "parser.mly"
                            ( ASTBPrim(Ast.Div, _3, _4) )
# 508 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 95 "parser.mly"
                            ( ASTBPrim(Ast.And, _3, _4) )
# 516 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 96 "parser.mly"
                           ( ASTBPrim(Ast.Or, _3, _4) )
# 524 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 97 "parser.mly"
                           ( ASTBPrim(Ast.Eq, _3, _4) )
# 532 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 98 "parser.mly"
                           ( ASTBPrim(Ast.Lt, _3, _4) )
# 540 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 99 "parser.mly"
                       ( ASTLambda(_2,_4) )
# 548 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.exprs) in
    Obj.repr(
# 100 "parser.mly"
                        ( ASTApply(_2,_3) )
# 556 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 101 "parser.mly"
                               ( ASTIf(_3,_4,_5) )
# 565 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 105 "parser.mly"
      ( ASTExpr(_1) )
# 572 "parser.ml"
               : Ast.exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.exprs) in
    Obj.repr(
# 106 "parser.mly"
              ( ASTExprs(_1,_2) )
# 580 "parser.ml"
               : Ast.exprs))
(* Entry prog *)
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
let prog (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.prog)
