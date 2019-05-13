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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Ast
# 48 "parser.ml"
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
  293 (* LEN *);
  294 (* NTH *);
  295 (* ALLOC *);
  296 (* VEC *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\011\000\002\000\002\000\002\000\003\000\003\000\003\000\
\003\000\003\000\012\000\012\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\010\000\
\010\000\004\000\004\000\004\000\004\000\004\000\004\000\005\000\
\005\000\005\000\005\000\006\000\006\000\007\000\008\000\008\000\
\000\000"

let yylen = "\002\000\
\003\000\003\000\001\000\003\000\003\000\002\000\003\000\004\000\
\003\000\003\000\001\000\005\000\001\000\001\000\001\000\001\000\
\004\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\004\000\004\000\006\000\004\000\004\000\005\000\001\000\
\002\000\004\000\007\000\008\000\003\000\006\000\007\000\001\000\
\001\000\005\000\004\000\001\000\003\000\003\000\001\000\003\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\049\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\015\000\016\000\013\000\014\000\000\000\000\000\006\000\000\000\
\000\000\000\000\000\000\000\000\000\000\011\000\000\000\000\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\040\000\
\041\000\000\000\000\000\000\000\000\000\037\000\000\000\000\000\
\000\000\007\000\000\000\000\000\009\000\000\000\010\000\005\000\
\004\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\034\000\000\000\000\000\
\000\000\000\000\000\000\000\000\008\000\033\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\017\000\000\000\
\029\000\000\000\030\000\027\000\046\000\048\000\026\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\002\000\
\018\000\019\000\020\000\021\000\022\000\023\000\024\000\025\000\
\000\000\031\000\043\000\045\000\000\000\000\000\000\000\038\000\
\000\000\012\000\028\000\042\000\035\000\000\000\039\000\036\000"

let yydgoto = "\002\000\
\004\000\014\000\015\000\016\000\092\000\093\000\054\000\055\000\
\070\000\071\000\068\000\032\000"

let yysindex = "\003\000\
\238\254\000\000\081\255\000\000\087\255\015\255\010\255\016\255\
\046\255\001\255\087\255\087\255\087\255\241\254\249\254\254\254\
\000\000\000\000\000\000\000\000\120\255\023\255\000\000\007\255\
\007\255\024\255\007\255\027\255\031\255\000\000\014\255\087\255\
\030\255\030\255\087\255\000\000\081\255\081\255\087\255\087\255\
\087\255\087\255\087\255\087\255\087\255\087\255\087\255\087\255\
\087\255\087\255\087\255\087\255\037\255\032\255\039\255\000\000\
\000\000\011\255\087\255\042\255\007\255\000\000\023\255\045\255\
\001\255\000\000\081\255\030\255\000\000\087\255\000\000\000\000\
\000\000\087\255\087\255\087\255\087\255\087\255\087\255\087\255\
\087\255\055\255\087\255\057\255\087\255\059\255\060\255\007\255\
\023\255\087\255\007\255\054\255\058\255\000\000\023\255\068\255\
\071\255\023\255\087\255\072\255\000\000\000\000\075\255\079\255\
\080\255\083\255\084\255\095\255\097\255\098\255\000\000\087\255\
\000\000\101\255\000\000\000\000\000\000\000\000\000\000\116\255\
\007\255\007\255\085\255\023\255\030\255\115\255\118\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\122\255\000\000\000\000\000\000\124\255\087\255\123\255\000\000\
\030\255\000\000\000\000\000\000\000\000\087\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\125\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\126\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\034\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\114\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\220\255\000\000\000\000\252\255\238\255\000\000\222\255\
\251\255\253\255\235\255\083\000"

let yytablesize = 159
let yytable = "\023\000\
\072\000\073\000\030\000\001\000\003\000\033\000\034\000\035\000\
\036\000\056\000\057\000\025\000\069\000\056\000\057\000\052\000\
\024\000\027\000\037\000\059\000\060\000\031\000\062\000\038\000\
\053\000\061\000\066\000\058\000\097\000\026\000\100\000\058\000\
\064\000\074\000\075\000\076\000\077\000\078\000\079\000\080\000\
\081\000\082\000\083\000\084\000\085\000\086\000\101\000\028\000\
\087\000\063\000\091\000\065\000\067\000\094\000\118\000\032\000\
\096\000\032\000\089\000\032\000\123\000\088\000\090\000\126\000\
\095\000\029\000\102\000\098\000\103\000\104\000\105\000\106\000\
\107\000\108\000\109\000\110\000\111\000\112\000\113\000\114\000\
\115\000\116\000\121\000\117\000\119\000\122\000\120\000\017\000\
\018\000\143\000\124\000\019\000\020\000\127\000\125\000\128\000\
\129\000\005\000\006\000\007\000\130\000\131\000\140\000\144\000\
\132\000\133\000\137\000\021\000\142\000\022\000\008\000\009\000\
\010\000\011\000\012\000\013\000\134\000\141\000\135\000\136\000\
\017\000\018\000\138\000\151\000\019\000\020\000\039\000\040\000\
\041\000\042\000\043\000\044\000\045\000\046\000\047\000\048\000\
\149\000\139\000\145\000\146\000\021\000\044\000\022\000\147\000\
\152\000\148\000\150\000\099\000\003\000\047\000\000\000\000\000\
\000\000\000\000\000\000\000\000\049\000\050\000\051\000"

let yycheck = "\005\000\
\037\000\038\000\002\001\001\000\023\001\011\000\012\000\013\000\
\024\001\003\001\004\001\002\001\034\000\003\001\004\001\021\000\
\002\001\002\001\026\001\024\000\025\000\021\001\027\000\026\001\
\002\001\002\001\032\000\021\001\063\000\020\001\067\000\021\001\
\002\001\039\000\040\000\041\000\042\000\043\000\044\000\045\000\
\046\000\047\000\048\000\049\000\050\000\051\000\068\000\002\001\
\052\000\023\001\040\001\038\001\023\001\059\000\089\000\022\001\
\061\000\024\001\027\001\026\001\095\000\025\001\024\001\098\000\
\023\001\020\001\070\000\023\001\074\000\075\000\076\000\077\000\
\078\000\079\000\080\000\081\000\022\001\083\000\022\001\085\000\
\022\001\022\001\029\001\088\000\090\000\028\001\091\000\001\001\
\002\001\124\000\023\001\005\001\006\001\099\000\024\001\024\001\
\022\001\017\001\018\001\019\001\022\001\022\001\121\000\125\000\
\022\001\022\001\112\000\021\001\024\001\023\001\030\001\031\001\
\032\001\033\001\034\001\035\001\022\001\122\000\022\001\022\001\
\001\001\002\001\022\001\145\000\005\001\006\001\007\001\008\001\
\009\001\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\142\000\022\001\024\001\022\001\021\001\028\001\023\001\022\001\
\150\000\022\001\024\001\065\000\024\001\024\001\255\255\255\255\
\255\255\255\255\255\255\255\255\037\001\038\001\039\001"

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
  LEN\000\
  NTH\000\
  ALLOC\000\
  VEC\000\
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
# 35 "parser.mly"
                ( ASTProg(_2) )
# 282 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmds) in
    Obj.repr(
# 40 "parser.mly"
                (ASTBlock(_2))
# 289 "parser.ml"
               : Ast.block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stat) in
    Obj.repr(
# 44 "parser.mly"
      (ASTStat(_1))
# 296 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.dec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 45 "parser.mly"
                      ( ASTDec(_1,_3) )
# 304 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 46 "parser.mly"
                       ( ASTStatcmd(_1,_3) )
# 312 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 50 "parser.mly"
           ( ASTEcho(_2) )
# 319 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'lval) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 52 "parser.mly"
               (ASTSet(_2,_3))
# 327 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.block) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 53 "parser.mly"
                       (ASTIfb (_2,_3,_4))
# 336 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 54 "parser.mly"
                   (ASTWhile(_2, _3))
# 344 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exprs) in
    Obj.repr(
# 55 "parser.mly"
                 (ASTCall(_2,_3))
# 352 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 60 "parser.mly"
       ( ASTLvId(_1))
# 359 "parser.ml"
               : 'lval))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'lval) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 61 "parser.mly"
                         (ASTLval(_3,_4))
# 367 "parser.ml"
               : 'lval))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "parser.mly"
      ( ASTTrue )
# 373 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "parser.mly"
         ( ASTFalse )
# 379 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 67 "parser.mly"
       ( ASTNum(_1) )
# 386 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 68 "parser.mly"
         ( ASTId(_1) )
# 393 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 69 "parser.mly"
                      ( ASTUPrim(Ast.Not,_3) )
# 400 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 70 "parser.mly"
                             ( ASTBPrim(Ast.Add, _3, _4) )
# 408 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 71 "parser.mly"
                              ( ASTBPrim(Ast.Sub, _3, _4) )
# 416 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 72 "parser.mly"
                              ( ASTBPrim(Ast.Mul, _3, _4) )
# 424 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 73 "parser.mly"
                            ( ASTBPrim(Ast.Div, _3, _4) )
# 432 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 74 "parser.mly"
                            ( ASTBPrim(Ast.And, _3, _4) )
# 440 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 75 "parser.mly"
                           ( ASTBPrim(Ast.Or, _3, _4) )
# 448 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 76 "parser.mly"
                           ( ASTBPrim(Ast.Eq, _3, _4) )
# 456 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 77 "parser.mly"
                           ( ASTBPrim(Ast.Lt, _3, _4) )
# 464 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 78 "parser.mly"
                       ( ASTLambda(_2,_4) )
# 472 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.exprs) in
    Obj.repr(
# 79 "parser.mly"
                        ( ASTApply(_2,_3) )
# 480 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 80 "parser.mly"
                               ( ASTIf(_3,_4,_5) )
# 489 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 82 "parser.mly"
                    (ASTLen(_3))
# 496 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 83 "parser.mly"
                      (ASTAlloc(_3))
# 503 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 84 "parser.mly"
                         (ASTNth(_3,_4))
# 511 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 88 "parser.mly"
      ( ASTExpr(_1) )
# 518 "parser.ml"
               : Ast.exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.exprs) in
    Obj.repr(
# 89 "parser.mly"
              ( ASTExprs(_1,_2) )
# 526 "parser.ml"
               : Ast.exprs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.tprim) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 94 "parser.mly"
                        ( ASTConst(_2,_3,_4) )
# 535 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.tprim) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 95 "parser.mly"
                                       ( ASTFun(_2,_3,_5,_7) )
# 545 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.tprim) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 96 "parser.mly"
                                           ( ASTFunRec(_3,_4,_6,_8) )
# 555 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.tprim) in
    Obj.repr(
# 98 "parser.mly"
                  (ASTVar(_2,_3))
# 563 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 99 "parser.mly"
                                   ( ASTProc(_2,_4,_6) )
# 572 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 100 "parser.mly"
                                       ( ASTProcRec(_3,_5,_7) )
# 581 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    Obj.repr(
# 103 "parser.mly"
     ( Int )
# 587 "parser.ml"
               : Ast.tprim))
; (fun __caml_parser_env ->
    Obj.repr(
# 104 "parser.mly"
        ( Bool )
# 593 "parser.ml"
               : Ast.tprim))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Ast.types) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.tprim) in
    Obj.repr(
# 105 "parser.mly"
                               ( ASTArrow(_2,_4) )
# 601 "parser.ml"
               : Ast.tprim))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.tprim) in
    Obj.repr(
# 107 "parser.mly"
                      (ASTVec(_3))
# 608 "parser.ml"
               : Ast.tprim))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.tprim) in
    Obj.repr(
# 111 "parser.mly"
       ( ASTType(_1) )
# 615 "parser.ml"
               : Ast.types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.tprim) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.types) in
    Obj.repr(
# 112 "parser.mly"
                    ( ASTTypes(_1,_3) )
# 623 "parser.ml"
               : Ast.types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.tprim) in
    Obj.repr(
# 116 "parser.mly"
                   ( Arg(_1,_3) )
# 631 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg) in
    Obj.repr(
# 120 "parser.mly"
     (ASTArg(_1) )
# 638 "parser.ml"
               : Ast.args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.args) in
    Obj.repr(
# 121 "parser.mly"
                 ( ASTArgs(_1,_3) )
# 646 "parser.ml"
               : Ast.args))
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
