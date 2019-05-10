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
\001\000\002\000\002\000\002\000\005\000\005\000\005\000\006\000\
\006\000\007\000\008\000\008\000\004\000\004\000\004\000\003\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\010\000\
\010\000\000\000"

let yylen = "\002\000\
\005\000\001\000\004\000\004\000\001\000\001\000\005\000\001\000\
\003\000\003\000\001\000\003\000\004\000\007\000\008\000\002\000\
\001\000\001\000\001\000\006\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\004\000\004\000\004\000\001\000\
\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\034\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\017\000\019\000\000\000\000\000\018\000\
\016\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\006\000\005\000\000\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
\000\000\000\000\004\000\003\000\000\000\000\000\000\000\000\000\
\000\000\029\000\000\000\000\000\000\000\000\000\033\000\031\000\
\010\000\012\000\030\000\000\000\000\000\000\000\000\000\000\000\
\021\000\022\000\023\000\024\000\025\000\026\000\027\000\028\000\
\009\000\000\000\000\000\000\000\020\000\007\000\014\000\000\000\
\015\000"

let yydgoto = "\002\000\
\004\000\009\000\010\000\011\000\062\000\063\000\036\000\037\000\
\057\000\058\000"

let yysindex = "\012\000\
\253\254\000\000\254\254\000\000\244\254\036\255\029\255\255\254\
\013\255\006\255\007\255\000\000\000\000\068\255\034\255\000\000\
\000\000\000\255\000\255\037\255\022\255\033\255\035\255\036\255\
\036\255\036\255\036\255\036\255\036\255\036\255\036\255\036\255\
\036\255\036\255\038\255\040\255\039\255\000\000\000\000\000\255\
\036\255\041\255\000\255\000\000\244\254\244\254\036\255\036\255\
\036\255\036\255\036\255\043\255\036\255\036\255\036\255\036\255\
\036\255\044\255\000\255\034\255\036\255\042\255\046\255\000\000\
\034\255\045\255\000\000\000\000\036\255\049\255\050\255\051\255\
\070\255\000\000\071\255\073\255\074\255\075\255\000\000\000\000\
\000\000\000\000\000\000\000\255\000\255\066\255\034\255\076\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\078\255\036\255\077\255\000\000\000\000\000\000\036\255\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\080\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\079\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\084\255\000\000\000\000\000\000\000\000\081\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\222\255\000\000\000\000\243\255\017\000\000\000\198\255\
\250\255\039\000"

let yytablesize = 106
let yytable = "\017\000\
\019\000\082\000\038\000\039\000\041\000\042\000\086\000\034\000\
\006\000\007\000\067\000\068\000\001\000\003\000\040\000\008\000\
\005\000\047\000\048\000\049\000\050\000\051\000\052\000\053\000\
\054\000\055\000\056\000\020\000\100\000\066\000\018\000\021\000\
\022\000\023\000\064\000\035\000\012\000\013\000\043\000\044\000\
\069\000\070\000\071\000\072\000\073\000\081\000\075\000\076\000\
\077\000\078\000\014\000\045\000\015\000\046\000\083\000\016\000\
\061\000\065\000\074\000\080\000\059\000\087\000\088\000\060\000\
\089\000\090\000\091\000\084\000\012\000\013\000\085\000\098\000\
\024\000\025\000\026\000\027\000\028\000\029\000\030\000\031\000\
\032\000\033\000\014\000\099\000\015\000\092\000\093\000\016\000\
\094\000\095\000\096\000\101\000\103\000\102\000\104\000\079\000\
\011\000\105\000\002\000\032\000\097\000\000\000\000\000\000\000\
\000\000\008\000"

let yycheck = "\006\000\
\002\001\060\000\003\001\004\001\018\000\019\000\065\000\014\000\
\021\001\022\001\045\000\046\000\001\000\017\001\015\001\028\001\
\019\001\024\000\025\000\026\000\027\000\028\000\029\000\030\000\
\031\000\032\000\033\000\029\001\087\000\043\000\002\001\019\001\
\027\001\027\001\041\000\002\001\001\001\002\001\002\001\018\001\
\047\000\048\000\049\000\050\000\051\000\059\000\053\000\054\000\
\055\000\056\000\015\001\019\001\017\001\019\001\061\000\020\001\
\018\001\017\001\016\001\016\001\023\001\017\001\069\000\024\001\
\016\001\016\001\016\001\026\001\001\001\002\001\025\001\085\000\
\005\001\006\001\007\001\008\001\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\018\001\017\001\016\001\016\001\020\001\
\016\001\016\001\016\001\016\001\099\000\016\001\018\001\057\000\
\018\001\104\000\019\001\016\001\084\000\255\255\255\255\255\255\
\255\255\025\001"

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
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 28 "parser.mly"
       ( _1 )
# 225 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'dec) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 29 "parser.mly"
                           (ASTCmds(_1,_4))
# 233 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'stat) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 30 "parser.mly"
                            (ASTCmds(_1,_4))
# 241 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 34 "parser.mly"
          (ASTType(_1))
# 248 "parser.ml"
               : 'mtype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 35 "parser.mly"
               ( ASTType(_1) )
# 255 "parser.ml"
               : 'mtype))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'mtypes) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'mtype) in
    Obj.repr(
# 36 "parser.mly"
                                 ( ASTFunType(_2,_4) )
# 263 "parser.ml"
               : 'mtype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'mtype) in
    Obj.repr(
# 40 "parser.mly"
        ( _1 )
# 270 "parser.ml"
               : 'mtypes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'mtype) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mtypes) in
    Obj.repr(
# 41 "parser.mly"
                      ( ASTSequenceType(_1,_3) )
# 278 "parser.ml"
               : 'mtypes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mtype) in
    Obj.repr(
# 45 "parser.mly"
                    ( ASTArg(ASTId(_1),_3) )
# 286 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 48 "parser.mly"
      ( _1)
# 293 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 49 "parser.mly"
                 (ASTListArg(_1,_3))
# 301 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'mtype) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 53 "parser.mly"
                        ( ASTDecConst(ASTId(_2),_3,_4))
# 310 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'mtype) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 54 "parser.mly"
                                        ( ASTFun(ASTId(_2),_3,_5,_7) )
# 320 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'mtype) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 55 "parser.mly"
                                            ( ASTFunRec(ASTId(_3),_4,_6,_8) )
# 330 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 59 "parser.mly"
           (ASTEcho(_2))
# 337 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 63 "parser.mly"
                             ( ASTNum(_1) )
# 344 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 64 "parser.mly"
                    ( ASTBool(_1) )
# 351 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 65 "parser.mly"
                               ( ASTId(_1) )
# 358 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 66 "parser.mly"
                               (ASTAlternative(_3,_4,_5))
# 367 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 67 "parser.mly"
                               ( ASTPrim(Ast.Add, _3, _4) )
# 375 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 68 "parser.mly"
                               ( ASTPrim(Ast.Sub, _3, _4) )
# 383 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 69 "parser.mly"
                               ( ASTPrim(Ast.Mul, _3, _4) )
# 391 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 70 "parser.mly"
                               ( ASTPrim(Ast.Div, _3, _4) )
# 399 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
                              ( ASTLog(Ast.And, _3, _4) )
# 407 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 72 "parser.mly"
                            ( ASTLog(Ast.Or, _3, _4) )
# 415 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 73 "parser.mly"
                            ( ASTLog(Ast.Eq, _3, _4) )
# 423 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 74 "parser.mly"
                              ( ASTLog(Ast.Lt, _3, _4) )
# 431 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 75 "parser.mly"
                          ( ASTNot(Ast.Not, _3) )
# 438 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 76 "parser.mly"
                        ( ASTFunAno(_2,_4) )
# 446 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 77 "parser.mly"
                         ( ASTSequence(_2,_3))
# 454 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 82 "parser.mly"
       (_1)
# 461 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 83 "parser.mly"
               ( ASTSequence(_1,_2) )
# 469 "parser.ml"
               : 'exprs))
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
