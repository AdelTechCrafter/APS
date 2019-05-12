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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Ast
# 37 "parser.ml"
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
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\003\000\004\000\004\000\004\000\
\005\000\005\000\005\000\006\000\006\000\007\000\008\000\008\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\010\000\010\000\000\000"

let yylen = "\002\000\
\003\000\001\000\003\000\003\000\002\000\004\000\007\000\008\000\
\001\000\001\000\005\000\001\000\003\000\003\000\001\000\003\000\
\001\000\001\000\001\000\001\000\004\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\004\000\004\000\006\000\
\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\035\000\000\000\000\000\000\000\000\000\
\000\000\000\000\019\000\020\000\017\000\018\000\000\000\000\000\
\005\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\009\000\010\000\000\000\
\000\000\000\000\000\000\004\000\003\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\006\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\021\000\000\000\034\000\031\000\014\000\016\000\030\000\
\000\000\000\000\000\000\000\000\022\000\023\000\024\000\025\000\
\026\000\027\000\028\000\029\000\000\000\013\000\000\000\000\000\
\000\000\032\000\011\000\007\000\000\000\008\000"

let yydgoto = "\002\000\
\004\000\008\000\009\000\010\000\061\000\062\000\036\000\037\000\
\056\000\057\000"

let yysindex = "\006\000\
\241\254\000\000\240\254\000\000\076\255\009\255\014\255\249\254\
\248\254\004\255\000\000\000\000\000\000\000\000\052\255\029\255\
\000\000\011\255\011\255\031\255\000\000\240\254\240\254\076\255\
\076\255\076\255\076\255\076\255\076\255\076\255\076\255\076\255\
\076\255\076\255\010\255\012\255\016\255\000\000\000\000\011\255\
\076\255\015\255\011\255\000\000\000\000\076\255\076\255\076\255\
\076\255\076\255\076\255\076\255\076\255\027\255\076\255\076\255\
\034\255\011\255\029\255\076\255\022\255\041\255\000\000\029\255\
\048\255\050\255\057\255\058\255\061\255\062\255\063\255\064\255\
\065\255\000\000\076\255\000\000\000\000\000\000\000\000\000\000\
\011\255\011\255\066\255\029\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\067\255\000\000\070\255\076\255\
\069\255\000\000\000\000\000\000\076\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\071\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\074\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\072\255\
\000\000\000\000\000\000\000\000\046\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\239\255\000\000\000\000\250\255\007\000\000\000\201\255\
\251\255\044\000"

let yytablesize = 100
let yytable = "\017\000\
\005\000\006\000\007\000\079\000\044\000\045\000\001\000\003\000\
\083\000\034\000\018\000\041\000\042\000\038\000\039\000\019\000\
\021\000\022\000\046\000\047\000\048\000\049\000\050\000\051\000\
\052\000\053\000\054\000\055\000\097\000\023\000\035\000\040\000\
\043\000\020\000\058\000\063\000\065\000\064\000\059\000\060\000\
\066\000\067\000\068\000\069\000\070\000\071\000\072\000\073\000\
\074\000\075\000\081\000\078\000\011\000\012\000\080\000\077\000\
\013\000\014\000\024\000\025\000\026\000\027\000\028\000\029\000\
\030\000\031\000\032\000\033\000\082\000\093\000\084\000\085\000\
\015\000\012\000\016\000\095\000\011\000\012\000\086\000\087\000\
\013\000\014\000\088\000\089\000\090\000\091\000\092\000\094\000\
\098\000\096\000\100\000\099\000\101\000\033\000\002\000\102\000\
\015\000\015\000\016\000\076\000"

let yycheck = "\005\000\
\017\001\018\001\019\001\059\000\022\000\023\000\001\000\023\001\
\064\000\015\000\002\001\018\000\019\000\003\001\004\001\002\001\
\024\001\026\001\024\000\025\000\026\000\027\000\028\000\029\000\
\030\000\031\000\032\000\033\000\084\000\026\001\002\001\021\001\
\002\001\020\001\025\001\041\000\043\000\023\001\027\001\024\001\
\046\000\047\000\048\000\049\000\050\000\051\000\052\000\053\000\
\022\001\055\000\029\001\058\000\001\001\002\001\060\000\022\001\
\005\001\006\001\007\001\008\001\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\028\001\075\000\023\001\022\001\
\021\001\028\001\023\001\082\000\001\001\002\001\022\001\022\001\
\005\001\006\001\022\001\022\001\022\001\022\001\022\001\081\000\
\022\001\024\001\096\000\022\001\024\001\022\001\024\001\101\000\
\021\001\024\001\023\001\056\000"

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
# 30 "parser.mly"
                ( ASTProg(_2) )
# 213 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stat) in
    Obj.repr(
# 36 "parser.mly"
      (ASTStat(_1))
# 220 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.dec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 37 "parser.mly"
                      ( ASTDec(_1,_3) )
# 228 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 38 "parser.mly"
                       ( ASTStatcmd(_1,_3) )
# 236 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 42 "parser.mly"
           ( ASTEcho(_2) )
# 243 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.tprim) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 46 "parser.mly"
                        ( ASTConst(_2,_3,_4) )
# 252 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.tprim) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 47 "parser.mly"
                                       ( ASTFun(_2,_3,_5,_7) )
# 262 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.tprim) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 48 "parser.mly"
                                           ( ASTFunRec(_3,_4,_6,_8) )
# 272 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "parser.mly"
     ( Int )
# 278 "parser.ml"
               : Ast.tprim))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parser.mly"
        ( Bool )
# 284 "parser.ml"
               : Ast.tprim))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Ast.types) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.tprim) in
    Obj.repr(
# 53 "parser.mly"
                               ( ASTArrow(_2,_4) )
# 292 "parser.ml"
               : Ast.tprim))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.tprim) in
    Obj.repr(
# 57 "parser.mly"
       ( ASTType(_1) )
# 299 "parser.ml"
               : Ast.types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.tprim) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.types) in
    Obj.repr(
# 58 "parser.mly"
                    ( ASTTypes(_1,_3) )
# 307 "parser.ml"
               : Ast.types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.tprim) in
    Obj.repr(
# 62 "parser.mly"
                   ( Arg(_1,_3) )
# 315 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg) in
    Obj.repr(
# 66 "parser.mly"
     (ASTArg(_1) )
# 322 "parser.ml"
               : Ast.args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.args) in
    Obj.repr(
# 67 "parser.mly"
                 ( ASTArgs(_1,_3) )
# 330 "parser.ml"
               : Ast.args))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "parser.mly"
      ( ASTTrue )
# 336 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
         ( ASTFalse )
# 342 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 73 "parser.mly"
       ( ASTNum(_1) )
# 349 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 74 "parser.mly"
         ( ASTId(_1) )
# 356 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 75 "parser.mly"
                      ( ASTUPrim(Ast.Not,_3) )
# 363 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 76 "parser.mly"
                             ( ASTBPrim(Ast.Add, _3, _4) )
# 371 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 77 "parser.mly"
                              ( ASTBPrim(Ast.Sub, _3, _4) )
# 379 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 78 "parser.mly"
                              ( ASTBPrim(Ast.Mul, _3, _4) )
# 387 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 79 "parser.mly"
                            ( ASTBPrim(Ast.Div, _3, _4) )
# 395 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 80 "parser.mly"
                            ( ASTBPrim(Ast.And, _3, _4) )
# 403 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 81 "parser.mly"
                           ( ASTBPrim(Ast.Or, _3, _4) )
# 411 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 82 "parser.mly"
                           ( ASTBPrim(Ast.Eq, _3, _4) )
# 419 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 83 "parser.mly"
                           ( ASTBPrim(Ast.Lt, _3, _4) )
# 427 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 84 "parser.mly"
                       ( ASTLambda(_2,_4) )
# 435 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.exprs) in
    Obj.repr(
# 85 "parser.mly"
                        ( ASTApply(_2,_3) )
# 443 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 86 "parser.mly"
                               ( ASTIf(_3,_4,_5) )
# 452 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 90 "parser.mly"
      ( ASTExpr(_1) )
# 459 "parser.ml"
               : Ast.exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.exprs) in
    Obj.repr(
# 91 "parser.mly"
              ( ASTExprs(_1,_2) )
# 467 "parser.ml"
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
