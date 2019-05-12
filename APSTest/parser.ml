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
\002\000\001\000\004\000\004\000\004\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\008\000\008\000\007\000\
\007\000\009\000\010\000\010\000\010\000\011\000\011\000\005\000\
\006\000\006\000\006\000\000\000"

let yylen = "\002\000\
\002\000\005\000\001\000\004\000\004\000\001\000\001\000\001\000\
\001\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\004\000\004\000\004\000\006\000\001\000\002\000\001\000\
\003\000\003\000\001\000\001\000\005\000\001\000\003\000\002\000\
\004\000\007\000\008\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\036\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\006\000\007\000\008\000\009\000\000\000\
\000\000\032\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\027\000\028\000\
\000\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\033\000\000\000\000\000\005\000\004\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\018\000\000\000\023\000\
\020\000\026\000\019\000\025\000\000\000\000\000\000\000\000\000\
\010\000\011\000\012\000\013\000\014\000\015\000\016\000\017\000\
\000\000\031\000\000\000\000\000\000\000\021\000\029\000\034\000\
\000\000\035\000"

let yydgoto = "\002\000\
\004\000\000\000\058\000\009\000\010\000\011\000\037\000\059\000\
\038\000\063\000\064\000"

let yysindex = "\002\000\
\237\254\000\000\233\254\000\000\060\255\080\255\007\255\000\255\
\240\254\244\254\248\254\000\000\000\000\000\000\000\000\056\255\
\016\255\000\000\013\255\013\255\030\255\012\255\004\255\005\255\
\080\255\080\255\080\255\080\255\080\255\080\255\080\255\080\255\
\080\255\080\255\080\255\017\255\019\255\014\255\000\000\000\000\
\013\255\080\255\021\255\013\255\000\000\060\255\060\255\080\255\
\080\255\080\255\080\255\080\255\080\255\080\255\080\255\031\255\
\080\255\080\255\033\255\013\255\080\255\016\255\028\255\034\255\
\000\000\016\255\036\255\000\000\000\000\041\255\059\255\061\255\
\064\255\065\255\070\255\071\255\072\255\000\000\080\255\000\000\
\000\000\000\000\000\000\000\000\013\255\013\255\054\255\016\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\073\255\000\000\074\255\080\255\075\255\000\000\000\000\000\000\
\080\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\067\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\081\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\076\255\000\000\000\000\000\000\000\000\078\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\250\255\215\255\000\000\000\000\198\255\043\000\
\000\000\249\255\019\000"

let yytablesize = 104
let yytable = "\018\000\
\003\000\020\000\001\000\084\000\068\000\069\000\005\000\087\000\
\019\000\035\000\023\000\042\000\043\000\022\000\024\000\039\000\
\040\000\036\000\048\000\049\000\050\000\051\000\052\000\053\000\
\054\000\055\000\056\000\057\000\021\000\101\000\041\000\044\000\
\045\000\046\000\047\000\065\000\067\000\062\000\060\000\061\000\
\066\000\070\000\071\000\072\000\073\000\074\000\075\000\076\000\
\077\000\078\000\079\000\081\000\082\000\085\000\083\000\088\000\
\012\000\013\000\086\000\089\000\014\000\015\000\025\000\026\000\
\027\000\028\000\029\000\030\000\031\000\032\000\033\000\034\000\
\097\000\016\000\100\000\017\000\006\000\090\000\099\000\091\000\
\012\000\013\000\092\000\093\000\014\000\015\000\007\000\008\000\
\094\000\095\000\096\000\102\000\103\000\104\000\022\000\105\000\
\003\000\016\000\106\000\017\000\080\000\024\000\030\000\098\000"

let yycheck = "\006\000\
\020\001\002\001\001\000\062\000\046\000\047\000\030\001\066\000\
\002\001\016\000\023\001\019\000\020\000\030\001\023\001\003\001\
\004\001\002\001\025\000\026\000\027\000\028\000\029\000\030\000\
\031\000\032\000\033\000\034\000\029\001\088\000\018\001\002\001\
\021\001\030\001\030\001\042\000\044\000\024\001\022\001\021\001\
\020\001\048\000\049\000\050\000\051\000\052\000\053\000\054\000\
\055\000\019\001\057\000\019\001\060\000\026\001\061\000\020\001\
\001\001\002\001\025\001\019\001\005\001\006\001\007\001\008\001\
\009\001\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\079\000\018\001\021\001\020\001\017\001\019\001\086\000\019\001\
\001\001\002\001\019\001\019\001\005\001\006\001\027\001\028\001\
\019\001\019\001\019\001\019\001\019\001\100\000\019\001\021\001\
\030\001\018\001\105\000\020\001\058\000\021\001\025\001\085\000"

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
# 21 "parser.mly"
          (_1)
# 219 "parser.ml"
               : 'line))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'cmds) in
    Obj.repr(
# 24 "parser.mly"
                        ( ASTProg(_3) )
# 226 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 27 "parser.mly"
      ( _1 )
# 233 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'decl) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 28 "parser.mly"
                           ( ASTDecs(_1,_4) )
# 241 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'stat) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 29 "parser.mly"
                           ( ASTStats(_1,_4) )
# 249 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 32 "parser.mly"
     ( ASTNum(_1) )
# 256 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 33 "parser.mly"
         ( ASTId(_1) )
# 263 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 34 "parser.mly"
        ( ASTBool(_1) )
# 270 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 35 "parser.mly"
         ( ASTBool(_1) )
# 277 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 36 "parser.mly"
                            ( ASTPrim(Ast.Add, _3, _4) )
# 285 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 37 "parser.mly"
                             ( ASTPrim(Ast.Sub, _3, _4) )
# 293 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 38 "parser.mly"
                             ( ASTPrim(Ast.Mul, _3, _4) )
# 301 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 39 "parser.mly"
                           ( ASTPrim(Ast.Div, _3, _4) )
# 309 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 40 "parser.mly"
                           ( ASTPrim(Ast.And, _3, _4) )
# 317 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 41 "parser.mly"
                          ( ASTPrim(Ast.Or, _3, _4) )
# 325 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 42 "parser.mly"
                          ( ASTPrim(Ast.Eq, _3, _4) )
# 333 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 43 "parser.mly"
                          ( ASTPrim(Ast.Lt, _3, _4) )
# 341 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 44 "parser.mly"
                      ( ASTUnaryPrim(Ast.Not, _3) )
# 348 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 45 "parser.mly"
                       ( ASTAbs(_2,_4) )
# 356 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 46 "parser.mly"
                        ( ASTApp(_2,_3) )
# 364 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 47 "parser.mly"
                               ( ASTIf(_3,_4,_5) )
# 373 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 51 "parser.mly"
      ( ASTSingle(_1) )
# 380 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 52 "parser.mly"
              ( ASTSequence(_1,_2) )
# 388 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 56 "parser.mly"
     ( _1 )
# 395 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 57 "parser.mly"
                 ( ASTArgs(_1,_3) )
# 403 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tprim) in
    Obj.repr(
# 60 "parser.mly"
                   ( ASTArg(ASTId(_1),_3) )
# 411 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "parser.mly"
     ( Int )
# 417 "parser.ml"
               : 'tprim))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "parser.mly"
        ( Bool )
# 423 "parser.ml"
               : 'tprim))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'types) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'tprim) in
    Obj.repr(
# 66 "parser.mly"
                               ( TypeFun(_2,_4) )
# 431 "parser.ml"
               : 'tprim))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tprim) in
    Obj.repr(
# 70 "parser.mly"
       ( _1 )
# 438 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'tprim) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'types) in
    Obj.repr(
# 71 "parser.mly"
                    ( Types(_1,_3) )
# 446 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 75 "parser.mly"
           ( ASTEcho(_2) )
# 453 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'tprim) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 79 "parser.mly"
                        ( ASTConst(_2,_3,_4) )
# 462 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'tprim) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 80 "parser.mly"
                                       ( ASTFun(_2,_3,_5,_7) )
# 472 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'tprim) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 81 "parser.mly"
                                           ( ASTFunRec(_3,_4,_6,_8) )
# 482 "parser.ml"
               : 'decl))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.expr)
