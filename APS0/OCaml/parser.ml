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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Ast
# 37 "parser.ml"
let yytransl_const = [|
  261 (* IF *);
  262 (* PLUS *);
  263 (* MINUS *);
  264 (* TIMES *);
  265 (* DIV *);
  266 (* NOT *);
  267 (* AND *);
  268 (* OR *);
  269 (* EQ *);
  270 (* LT *);
  271 (* LPAR *);
  272 (* RPAR *);
  273 (* LCRO *);
  274 (* RCRO *);
  275 (* EOL *);
  277 (* ECHO *);
  278 (* CONST *);
  279 (* COLON *);
  280 (* COMA *);
  281 (* ARROW *);
  282 (* STAR *);
  283 (* SEMICOLON *);
  284 (* FUN *);
  285 (* REC *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
  259 (* TYPEBOOL *);
  260 (* TYPENUM *);
  276 (* BOOL *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\005\000\005\000\006\000\007\000\007\000\007\000\008\000\008\000\
\009\000\009\000\009\000\009\000\010\000\004\000\004\000\002\000\
\002\000\002\000\000\000"

let yylen = "\002\000\
\005\000\001\000\001\000\001\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\004\000\004\000\004\000\006\000\
\001\000\002\000\002\000\001\000\001\000\005\000\001\000\003\000\
\004\000\003\000\007\000\008\000\003\000\001\000\003\000\001\000\
\004\000\004\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\035\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\021\000\020\000\000\000\000\000\
\002\000\004\000\000\000\000\000\003\000\019\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\026\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\025\000\000\000\000\000\034\000\
\033\000\024\000\000\000\000\000\000\000\000\000\000\000\000\000\
\013\000\000\000\000\000\000\000\000\000\018\000\015\000\029\000\
\014\000\031\000\000\000\000\000\022\000\000\000\005\000\006\000\
\007\000\008\000\009\000\010\000\011\000\012\000\000\000\000\000\
\016\000\027\000\000\000\028\000"

let yydgoto = "\002\000\
\004\000\010\000\064\000\044\000\065\000\011\000\029\000\030\000\
\012\000\045\000"

let yysindex = "\004\000\
\245\254\000\000\244\254\000\000\036\255\007\255\000\255\011\255\
\006\255\251\254\250\254\252\254\000\000\000\000\007\255\000\255\
\000\000\000\000\075\255\022\255\000\000\000\000\007\255\007\255\
\039\255\024\255\025\255\026\255\017\255\021\255\000\000\000\255\
\000\255\000\255\000\255\000\255\000\255\000\255\000\255\000\255\
\000\255\000\255\029\255\041\255\038\255\000\255\046\255\007\255\
\000\000\036\255\036\255\007\255\007\255\000\255\000\255\000\255\
\000\255\000\255\049\255\000\255\000\255\000\255\000\255\000\255\
\051\255\007\255\000\255\022\255\000\000\022\255\053\255\000\000\
\000\000\000\000\052\255\000\255\055\255\056\255\057\255\058\255\
\000\000\059\255\062\255\077\255\078\255\000\000\000\000\000\000\
\000\000\000\000\073\255\022\255\000\000\081\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\255\080\255\
\000\000\000\000\000\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\082\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\074\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\084\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\087\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\209\255\249\255\204\255\040\000\000\000\013\000\053\000\
\000\000\000\000"

let yytablesize = 105
let yytable = "\022\000\
\017\000\018\000\072\000\073\000\001\000\003\000\005\000\024\000\
\031\000\013\000\014\000\042\000\023\000\026\000\019\000\090\000\
\020\000\091\000\016\000\021\000\027\000\015\000\028\000\043\000\
\054\000\055\000\056\000\057\000\058\000\059\000\060\000\061\000\
\062\000\063\000\025\000\046\000\047\000\006\000\069\000\104\000\
\048\000\049\000\052\000\050\000\051\000\053\000\076\000\077\000\
\078\000\079\000\080\000\066\000\082\000\083\000\084\000\085\000\
\007\000\008\000\067\000\089\000\071\000\068\000\070\000\009\000\
\081\000\075\000\087\000\093\000\094\000\092\000\095\000\096\000\
\097\000\098\000\099\000\017\000\018\000\100\000\088\000\032\000\
\033\000\034\000\035\000\036\000\037\000\038\000\039\000\040\000\
\041\000\019\000\103\000\020\000\101\000\102\000\021\000\106\000\
\105\000\107\000\023\000\108\000\032\000\030\000\017\000\086\000\
\074\000"

let yycheck = "\007\000\
\001\001\002\001\050\000\051\000\001\000\017\001\019\001\002\001\
\016\000\003\001\004\001\019\000\002\001\019\001\015\001\068\000\
\017\001\070\000\006\000\020\001\027\001\015\001\027\001\002\001\
\032\000\033\000\034\000\035\000\036\000\037\000\038\000\039\000\
\040\000\041\000\029\001\023\000\024\000\002\001\046\000\092\000\
\002\001\018\001\026\001\019\001\019\001\025\001\054\000\055\000\
\056\000\057\000\058\000\023\001\060\000\061\000\062\000\063\000\
\021\001\022\001\018\001\067\000\048\000\024\001\017\001\028\001\
\016\001\053\000\016\001\016\001\076\000\017\001\016\001\016\001\
\016\001\016\001\016\001\001\001\002\001\016\001\066\000\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\018\001\017\001\016\001\016\001\020\001\103\000\
\016\001\018\001\025\001\107\000\019\001\018\001\016\001\064\000\
\052\000"

let yynames_const = "\
  IF\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  NOT\000\
  AND\000\
  OR\000\
  EQ\000\
  LT\000\
  LPAR\000\
  RPAR\000\
  LCRO\000\
  RCRO\000\
  EOL\000\
  ECHO\000\
  CONST\000\
  COLON\000\
  COMA\000\
  ARROW\000\
  STAR\000\
  SEMICOLON\000\
  FUN\000\
  REC\000\
  "

let yynames_block = "\
  NUM\000\
  IDENT\000\
  TYPEBOOL\000\
  TYPENUM\000\
  BOOL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'cmds) in
    Obj.repr(
# 24 "parser.mly"
                         ( ASTProg(_3) )
# 218 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 29 "parser.mly"
                             ( ASTNum(_1) )
# 225 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 30 "parser.mly"
                   ( ASTBool(_1) )
# 232 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 31 "parser.mly"
                              ( ASTId(_1) )
# 239 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 32 "parser.mly"
                              ( ASTPrim(Ast.Add, _3, _4) )
# 247 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 33 "parser.mly"
                              ( ASTPrim(Ast.Sub, _3, _4) )
# 255 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 34 "parser.mly"
                              ( ASTPrim(Ast.Mul, _3, _4) )
# 263 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 35 "parser.mly"
                              ( ASTPrim(Ast.Div, _3, _4) )
# 271 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 36 "parser.mly"
                             ( ASTLog(Ast.And, _3, _4) )
# 279 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 37 "parser.mly"
                           ( ASTLog(Ast.Or, _3, _4) )
# 287 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 38 "parser.mly"
                           ( ASTLog(Ast.Eq, _3, _4) )
# 295 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 39 "parser.mly"
                             ( ASTLog(Ast.Lt, _3, _4) )
# 303 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 40 "parser.mly"
                         ( ASTNot(Ast.Not, _3) )
# 310 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 41 "parser.mly"
                       ( ASTFunAno(_2,_4) )
# 318 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 42 "parser.mly"
                        ( ASTSequence(_2,_3))
# 326 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 43 "parser.mly"
                              (ASTAlternative(_3,_4,_5))
# 335 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 47 "parser.mly"
       (_1)
# 342 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 48 "parser.mly"
              ( ASTSequence(_1,_2) )
# 350 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 52 "parser.mly"
           (ASTEcho(_2))
# 357 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 55 "parser.mly"
          (ASTType(_1))
# 364 "parser.ml"
               : 'mtype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 56 "parser.mly"
               ( ASTType(_1) )
# 371 "parser.ml"
               : 'mtype))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'mtypes) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'mtype) in
    Obj.repr(
# 57 "parser.mly"
                                 ( ASTFunType(_2,_4) )
# 379 "parser.ml"
               : 'mtype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'mtype) in
    Obj.repr(
# 60 "parser.mly"
        ( _1 )
# 386 "parser.ml"
               : 'mtypes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'mtype) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mtypes) in
    Obj.repr(
# 61 "parser.mly"
                      ( ASTSequenceType(_1,_3) )
# 394 "parser.ml"
               : 'mtypes))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'mtype) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 63 "parser.mly"
                        ( ASTDecConst(ASTId(_2),_3,_4))
# 403 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'mtype) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 64 "parser.mly"
                    ( ASTDec(ASTId(_1),_2,_3))
# 412 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'mtype) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 65 "parser.mly"
                                        ( ASTFun(ASTId(_2),_3,_5,_7) )
# 422 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'mtype) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 66 "parser.mly"
                                            ( ASTFunRec(ASTId(_3),_4,_6,_8) )
# 432 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mtype) in
    Obj.repr(
# 69 "parser.mly"
                    ( ASTArg(ASTId(_1),_3) )
# 440 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 72 "parser.mly"
      ( _1)
# 447 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 73 "parser.mly"
                 (ASTListArg(_1,_3))
# 455 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 77 "parser.mly"
       ( _1 )
# 462 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'dec) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 78 "parser.mly"
                           (ASTCmds(_1,_4))
# 470 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'stat) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 79 "parser.mly"
                            (ASTCmds(_1,_4))
# 478 "parser.ml"
               : 'cmds))
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
