type token =
  | IF of (Support.Error.info)
  | THEN of (Support.Error.info)
  | ELSE of (Support.Error.info)
  | TRUE of (Support.Error.info)
  | FALSE of (Support.Error.info)
  | BOOL of (Support.Error.info)
  | LAMBDA of (Support.Error.info)
  | TIMESFLOAT of (Support.Error.info)
  | SUCC of (Support.Error.info)
  | PRED of (Support.Error.info)
  | ISZERO of (Support.Error.info)
  | NAT of (Support.Error.info)
  | LET of (Support.Error.info)
  | LETREC of (Support.Error.info)
  | IN of (Support.Error.info)
  | UCID of (string Support.Error.withinfo)
  | LCID of (string Support.Error.withinfo)
  | INTV of (int Support.Error.withinfo)
  | FLOATV of (float Support.Error.withinfo)
  | STRINGV of (string Support.Error.withinfo)
  | APOSTROPHE of (Support.Error.info)
  | DQUOTE of (Support.Error.info)
  | ARROW of (Support.Error.info)
  | BANG of (Support.Error.info)
  | BARGT of (Support.Error.info)
  | BARRCURLY of (Support.Error.info)
  | BARRSQUARE of (Support.Error.info)
  | COLON of (Support.Error.info)
  | COLONCOLON of (Support.Error.info)
  | COLONEQ of (Support.Error.info)
  | COLONHASH of (Support.Error.info)
  | COMMA of (Support.Error.info)
  | DARROW of (Support.Error.info)
  | DDARROW of (Support.Error.info)
  | DOT of (Support.Error.info)
  | EOF of (Support.Error.info)
  | EQ of (Support.Error.info)
  | EQEQ of (Support.Error.info)
  | EXISTS of (Support.Error.info)
  | GT of (Support.Error.info)
  | HASH of (Support.Error.info)
  | LCURLY of (Support.Error.info)
  | LCURLYBAR of (Support.Error.info)
  | LEFTARROW of (Support.Error.info)
  | LPAREN of (Support.Error.info)
  | LSQUARE of (Support.Error.info)
  | LSQUAREBAR of (Support.Error.info)
  | LT of (Support.Error.info)
  | RCURLY of (Support.Error.info)
  | RPAREN of (Support.Error.info)
  | RSQUARE of (Support.Error.info)
  | SEMI of (Support.Error.info)
  | SLASH of (Support.Error.info)
  | STAR of (Support.Error.info)
  | TRIANGLE of (Support.Error.info)
  | USCORE of (Support.Error.info)
  | VBAR of (Support.Error.info)

open Parsing;;
let _ = parse_error;;
# 14 "parser.mly"
open Support.Error
open Support.Pervasive
open Syntax
# 67 "parser.ml"
let yytransl_const = [|
    0|]

let yytransl_block = [|
  257 (* IF *);
  258 (* THEN *);
  259 (* ELSE *);
  260 (* TRUE *);
  261 (* FALSE *);
  262 (* BOOL *);
  263 (* LAMBDA *);
  264 (* TIMESFLOAT *);
  265 (* SUCC *);
  266 (* PRED *);
  267 (* ISZERO *);
  268 (* NAT *);
  269 (* LET *);
  270 (* LETREC *);
  271 (* IN *);
  272 (* UCID *);
  273 (* LCID *);
  274 (* INTV *);
  275 (* FLOATV *);
  276 (* STRINGV *);
  277 (* APOSTROPHE *);
  278 (* DQUOTE *);
  279 (* ARROW *);
  280 (* BANG *);
  281 (* BARGT *);
  282 (* BARRCURLY *);
  283 (* BARRSQUARE *);
  284 (* COLON *);
  285 (* COLONCOLON *);
  286 (* COLONEQ *);
  287 (* COLONHASH *);
  288 (* COMMA *);
  289 (* DARROW *);
  290 (* DDARROW *);
  291 (* DOT *);
    0 (* EOF *);
  292 (* EQ *);
  293 (* EQEQ *);
  294 (* EXISTS *);
  295 (* GT *);
  296 (* HASH *);
  297 (* LCURLY *);
  298 (* LCURLYBAR *);
  299 (* LEFTARROW *);
  300 (* LPAREN *);
  301 (* LSQUARE *);
  302 (* LSQUAREBAR *);
  303 (* LT *);
  304 (* RCURLY *);
  305 (* RPAREN *);
  306 (* RSQUARE *);
  307 (* SEMI *);
  308 (* SLASH *);
  309 (* STAR *);
  310 (* TRIANGLE *);
  311 (* USCORE *);
  312 (* VBAR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\004\000\004\000\004\000\005\000\
\007\000\007\000\007\000\006\000\006\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\008\000\008\000\008\000\
\008\000\008\000\008\000\009\000\009\000\009\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\011\000\011\000\
\012\000\012\000\013\000\013\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\002\000\001\000\002\000\002\000\001\000\
\003\000\001\000\001\000\003\000\001\000\001\000\006\000\006\000\
\006\000\008\000\008\000\006\000\006\000\001\000\002\000\003\000\
\002\000\002\000\002\000\003\000\003\000\001\000\003\000\001\000\
\001\000\001\000\003\000\001\000\001\000\001\000\000\000\001\000\
\001\000\003\000\003\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\032\000\033\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\038\000\036\000\037\000\
\001\000\000\000\000\000\045\000\000\000\003\000\000\000\000\000\
\030\000\034\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\005\000\
\004\000\000\000\044\000\000\000\040\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\010\000\011\000\000\000\007\000\008\000\000\000\
\006\000\000\000\035\000\000\000\031\000\002\000\028\000\029\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\043\000\042\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\009\000\012\000\015\000\016\000\017\000\020\000\
\021\000\000\000\000\000\000\000\000\000\018\000\019\000"

let yydgoto = "\002\000\
\020\000\021\000\022\000\041\000\062\000\063\000\064\000\023\000\
\024\000\025\000\044\000\045\000\046\000"

let yysindex = "\002\000\
\001\000\000\000\054\000\000\000\000\000\241\254\015\255\015\255\
\015\255\015\255\243\254\252\254\043\255\000\000\000\000\000\000\
\000\000\074\000\054\000\000\000\212\254\000\000\015\255\233\254\
\000\000\000\000\012\255\245\254\246\254\085\000\233\254\233\254\
\233\254\242\254\247\254\009\255\011\255\056\255\054\000\000\000\
\000\000\013\255\000\000\016\255\000\000\028\255\017\255\001\000\
\233\254\244\254\054\000\056\255\056\255\233\254\054\000\054\000\
\056\255\056\255\000\000\000\000\056\255\000\000\000\000\038\255\
\000\000\054\000\000\000\074\000\000\000\000\000\000\000\000\000\
\066\255\037\255\041\255\058\255\063\255\044\255\057\255\045\255\
\056\255\000\000\000\000\054\000\054\000\054\000\054\000\054\000\
\054\000\054\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\077\255\081\255\054\000\054\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\203\255\000\000\000\000\000\000\
\000\000\051\255\000\000\000\000\000\000\000\000\168\255\006\255\
\000\000\000\000\000\000\000\000\000\000\000\000\026\255\086\255\
\106\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\012\000\000\000\000\000\000\000\054\255\000\000\000\000\
\141\255\000\000\000\000\000\000\000\000\161\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\152\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\059\000\000\000\253\255\000\000\172\000\031\000\000\000\000\000\
\106\000\000\000\000\000\049\000\000\000"

let yytablesize = 385
let yytable = "\027\000\
\017\000\028\000\001\000\034\000\071\000\072\000\048\000\022\000\
\022\000\022\000\022\000\050\000\036\000\051\000\043\000\047\000\
\052\000\053\000\004\000\005\000\022\000\055\000\022\000\022\000\
\022\000\022\000\056\000\025\000\025\000\025\000\025\000\026\000\
\014\000\015\000\016\000\065\000\057\000\022\000\058\000\029\000\
\025\000\035\000\025\000\025\000\025\000\025\000\022\000\073\000\
\066\000\022\000\037\000\076\000\077\000\022\000\022\000\018\000\
\022\000\025\000\019\000\068\000\081\000\059\000\082\000\067\000\
\043\000\069\000\025\000\060\000\084\000\025\000\038\000\085\000\
\087\000\025\000\025\000\086\000\025\000\088\000\039\000\089\000\
\093\000\094\000\095\000\096\000\097\000\098\000\099\000\026\000\
\026\000\026\000\026\000\100\000\090\000\091\000\040\000\101\000\
\102\000\103\000\039\000\061\000\026\000\041\000\026\000\026\000\
\026\000\026\000\070\000\027\000\027\000\027\000\027\000\092\000\
\030\000\031\000\032\000\033\000\083\000\026\000\000\000\000\000\
\027\000\000\000\027\000\027\000\027\000\027\000\026\000\000\000\
\049\000\026\000\000\000\000\000\000\000\026\000\026\000\054\000\
\026\000\027\000\000\000\000\000\000\000\000\000\023\000\023\000\
\023\000\023\000\027\000\000\000\000\000\027\000\000\000\000\000\
\000\000\027\000\027\000\023\000\027\000\023\000\023\000\023\000\
\023\000\000\000\024\000\024\000\024\000\024\000\000\000\000\000\
\000\000\014\000\014\000\000\000\023\000\000\000\000\000\024\000\
\000\000\024\000\024\000\024\000\024\000\023\000\014\000\000\000\
\023\000\000\000\013\000\013\000\023\000\023\000\000\000\023\000\
\024\000\000\000\000\000\000\000\000\000\000\000\000\000\014\000\
\013\000\024\000\013\000\000\000\024\000\000\000\034\000\034\000\
\024\000\024\000\000\000\024\000\000\000\000\000\000\000\014\000\
\014\000\000\000\014\000\034\000\034\000\034\000\034\000\074\000\
\075\000\000\000\000\000\000\000\078\000\079\000\000\000\000\000\
\080\000\000\000\000\000\000\000\000\000\034\000\000\000\000\000\
\000\000\000\000\000\000\034\000\000\000\000\000\034\000\000\000\
\000\000\000\000\000\000\000\000\000\000\034\000\000\000\000\000\
\000\000\003\000\000\000\000\000\004\000\005\000\000\000\006\000\
\007\000\008\000\009\000\010\000\000\000\011\000\012\000\034\000\
\034\000\013\000\014\000\015\000\016\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\034\000\034\000\034\000\034\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\018\000\000\000\034\000\019\000\000\000\034\000\000\000\
\000\000\000\000\000\000\000\000\034\000\000\000\003\000\034\000\
\000\000\004\000\005\000\034\000\006\000\007\000\008\000\009\000\
\010\000\000\000\011\000\012\000\000\000\000\000\026\000\014\000\
\015\000\016\000\003\000\000\000\000\000\004\000\005\000\000\000\
\006\000\007\000\008\000\009\000\010\000\000\000\011\000\012\000\
\004\000\005\000\042\000\014\000\015\000\016\000\018\000\000\000\
\000\000\019\000\000\000\000\000\000\000\026\000\014\000\015\000\
\016\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\018\000\000\000\000\000\019\000\000\000\050\000\
\000\000\000\000\000\000\000\000\000\000\018\000\000\000\000\000\
\019\000"

let yycheck = "\003\000\
\000\000\017\001\001\000\017\001\017\001\018\001\051\001\002\001\
\003\001\004\001\005\001\035\001\017\001\002\001\018\000\019\000\
\028\001\028\001\004\001\005\001\015\001\036\001\017\001\018\001\
\019\001\020\001\036\001\002\001\003\001\004\001\005\001\017\001\
\018\001\019\001\020\001\039\000\028\001\032\001\028\001\055\001\
\015\001\055\001\017\001\018\001\019\001\020\001\041\001\051\000\
\036\001\044\001\055\001\055\000\056\000\048\001\049\001\041\001\
\051\001\032\001\044\001\032\001\023\001\006\001\066\000\048\001\
\068\000\049\001\041\001\012\001\003\001\044\001\028\001\035\001\
\015\001\048\001\049\001\035\001\051\001\015\001\036\001\036\001\
\084\000\085\000\086\000\087\000\088\000\089\000\090\000\002\001\
\003\001\004\001\005\001\015\001\036\001\049\001\052\001\015\001\
\100\000\101\000\048\001\044\001\015\001\048\001\017\001\018\001\
\019\001\020\001\048\000\002\001\003\001\004\001\005\001\081\000\
\007\000\008\000\009\000\010\000\068\000\032\001\255\255\255\255\
\015\001\255\255\017\001\018\001\019\001\020\001\041\001\255\255\
\023\000\044\001\255\255\255\255\255\255\048\001\049\001\030\000\
\051\001\032\001\255\255\255\255\255\255\255\255\002\001\003\001\
\004\001\005\001\041\001\255\255\255\255\044\001\255\255\255\255\
\255\255\048\001\049\001\015\001\051\001\017\001\018\001\019\001\
\020\001\255\255\002\001\003\001\004\001\005\001\255\255\255\255\
\255\255\002\001\003\001\255\255\032\001\255\255\255\255\015\001\
\255\255\017\001\018\001\019\001\020\001\041\001\015\001\255\255\
\044\001\255\255\035\001\036\001\048\001\049\001\255\255\051\001\
\032\001\255\255\255\255\255\255\255\255\255\255\255\255\032\001\
\049\001\041\001\051\001\255\255\044\001\255\255\004\001\005\001\
\048\001\049\001\255\255\051\001\255\255\255\255\255\255\048\001\
\049\001\255\255\051\001\017\001\018\001\019\001\020\001\052\000\
\053\000\255\255\255\255\255\255\057\000\058\000\255\255\255\255\
\061\000\255\255\255\255\255\255\255\255\035\001\255\255\255\255\
\255\255\255\255\255\255\041\001\255\255\255\255\044\001\255\255\
\255\255\255\255\255\255\255\255\255\255\051\001\255\255\255\255\
\255\255\001\001\255\255\255\255\004\001\005\001\255\255\007\001\
\008\001\009\001\010\001\011\001\255\255\013\001\014\001\004\001\
\005\001\017\001\018\001\019\001\020\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\017\001\018\001\019\001\020\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\041\001\255\255\032\001\044\001\255\255\035\001\255\255\
\255\255\255\255\255\255\255\255\041\001\255\255\001\001\044\001\
\255\255\004\001\005\001\048\001\007\001\008\001\009\001\010\001\
\011\001\255\255\013\001\014\001\255\255\255\255\017\001\018\001\
\019\001\020\001\001\001\255\255\255\255\004\001\005\001\255\255\
\007\001\008\001\009\001\010\001\011\001\255\255\013\001\014\001\
\004\001\005\001\017\001\018\001\019\001\020\001\041\001\255\255\
\255\255\044\001\255\255\255\255\255\255\017\001\018\001\019\001\
\020\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\041\001\255\255\255\255\044\001\255\255\035\001\
\255\255\255\255\255\255\255\255\255\255\041\001\255\255\255\255\
\044\001"

let yynames_const = "\
  "

let yynames_block = "\
  IF\000\
  THEN\000\
  ELSE\000\
  TRUE\000\
  FALSE\000\
  BOOL\000\
  LAMBDA\000\
  TIMESFLOAT\000\
  SUCC\000\
  PRED\000\
  ISZERO\000\
  NAT\000\
  LET\000\
  LETREC\000\
  IN\000\
  UCID\000\
  LCID\000\
  INTV\000\
  FLOATV\000\
  STRINGV\000\
  APOSTROPHE\000\
  DQUOTE\000\
  ARROW\000\
  BANG\000\
  BARGT\000\
  BARRCURLY\000\
  BARRSQUARE\000\
  COLON\000\
  COLONCOLON\000\
  COLONEQ\000\
  COLONHASH\000\
  COMMA\000\
  DARROW\000\
  DDARROW\000\
  DOT\000\
  EOF\000\
  EQ\000\
  EQEQ\000\
  EXISTS\000\
  GT\000\
  HASH\000\
  LCURLY\000\
  LCURLYBAR\000\
  LEFTARROW\000\
  LPAREN\000\
  LSQUARE\000\
  LSQUAREBAR\000\
  LT\000\
  RCURLY\000\
  RPAREN\000\
  RSQUARE\000\
  SEMI\000\
  SLASH\000\
  STAR\000\
  TRIANGLE\000\
  USCORE\000\
  VBAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 123 "parser.mly"
      ( fun ctx -> [],ctx )
# 373 "parser.ml"
               :  Syntax.context -> (Syntax.command list * Syntax.context) ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Command) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Syntax.context -> (Syntax.command list * Syntax.context) ) in
    Obj.repr(
# 128 "parser.mly"
      ( fun ctx ->
          let cmd,ctx = _1 ctx in
          let cmds,ctx = _3 ctx in
          cmd::cmds,ctx )
# 385 "parser.ml"
               :  Syntax.context -> (Syntax.command list * Syntax.context) ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 138 "parser.mly"
      ( fun ctx -> (let t = _1 ctx in Eval(tmInfo t,t)),ctx )
# 392 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Binder) in
    Obj.repr(
# 143 "parser.mly"
      ( fun ctx -> ((Bind(_1.i,_1.v,_2 ctx)), addname ctx _1.v) )
# 400 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 150 "parser.mly"
      ( fun ctx -> NameBind )
# 407 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 154 "parser.mly"
      ( fun ctx -> TmAbbBind(_2 ctx, None) )
# 415 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 156 "parser.mly"
    ( fun ctx -> VarBind (_2 ctx))
# 423 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ArrowType) in
    Obj.repr(
# 160 "parser.mly"
                ( _1 )
# 430 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 165 "parser.mly"
           ( _2 )
# 439 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 167 "parser.mly"
      ( fun ctx -> TyBool )
# 446 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 169 "parser.mly"
      ( fun ctx -> TyNat )
# 453 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'AType) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ArrowType) in
    Obj.repr(
# 176 "parser.mly"
           ( fun ctx -> TyArr(_1 ctx, _3 ctx) )
# 462 "parser.ml"
               : 'ArrowType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AType) in
    Obj.repr(
# 178 "parser.mly"
                  ( _1 )
# 469 "parser.ml"
               : 'ArrowType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AppTerm) in
    Obj.repr(
# 190 "parser.mly"
      ( _1 )
# 476 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 194 "parser.mly"
      ( fun ctx -> TmIf(_1, _2 ctx, _4 ctx, _6 ctx) )
# 488 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Type) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 198 "parser.mly"
      ( fun ctx ->
          let ctx1 = addname ctx _2.v in
          TmAbs(_1, _2.v, _4 ctx, _6 ctx1) )
# 502 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Type) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 203 "parser.mly"
      ( fun ctx ->
          let ctx1 = addname ctx "_" in
          TmAbs(_1, "_", _4 ctx, _6 ctx1) )
# 516 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'Type) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 210 "parser.mly"
      ( fun ctx -> TmLet(_1, _2.v, TmApp(_1,fix TyNat _1 _2.v ctx,TmAbs(_1,_2.v,TyNat,_6 (addname ctx _2.v))), _8 (addname ctx _2.v)))
# 530 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 6 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'Type) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 213 "parser.mly"
      ( fun ctx -> TmLet(_1, "_", _6 ctx, _8 (addname ctx "_")) )
# 544 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 217 "parser.mly"
      ( fun ctx -> TmLet(_1, _2.v, _4 ctx, _6 (addname ctx _2.v)) )
# 556 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 220 "parser.mly"
      ( fun ctx -> TmLet(_1, "_", _4 ctx, _6 (addname ctx "_")) )
# 568 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 225 "parser.mly"
      ( _1 )
# 575 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'AppTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 228 "parser.mly"
      ( fun ctx ->
          let e1 = _1 ctx in
          let e2 = _2 ctx in
          TmApp(tmInfo e1,e1,e2) )
# 586 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'PathTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 234 "parser.mly"
      ( fun ctx -> TmTimesfloat(_1, _2 ctx, _3 ctx) )
# 595 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 237 "parser.mly"
      ( fun ctx -> TmSucc(_1, _2 ctx) )
# 603 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 240 "parser.mly"
      ( fun ctx -> TmPred(_1, _2 ctx) )
# 611 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 243 "parser.mly"
      ( fun ctx -> TmIsZero(_1, _2 ctx) )
# 619 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'PathTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string Support.Error.withinfo) in
    Obj.repr(
# 249 "parser.mly"
      ( fun ctx ->
          TmProj(_2, _1 ctx, _3.v) )
# 629 "parser.ml"
               : 'PathTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'PathTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int Support.Error.withinfo) in
    Obj.repr(
# 254 "parser.mly"
      ( fun ctx ->
          TmProj(_2, _1 ctx, string_of_int _3.v) )
# 639 "parser.ml"
               : 'PathTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 258 "parser.mly"
      ( _1 )
# 646 "parser.ml"
               : 'PathTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 264 "parser.mly"
      ( _2 )
# 655 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 267 "parser.mly"
      ( fun ctx -> TmTrue(_1) )
# 662 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 270 "parser.mly"
      ( fun ctx -> TmFalse(_1) )
# 669 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Support.Error.withinfo) in
    Obj.repr(
# 274 "parser.mly"
      ( fun ctx ->
          TmVar(_1.i, name2index _1.i ctx _1.v, ctxlength ctx) )
# 677 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Fields) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 278 "parser.mly"
      ( fun ctx ->
          TmRecord(_1, _2 ctx 1) )
# 687 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float Support.Error.withinfo) in
    Obj.repr(
# 282 "parser.mly"
      ( fun ctx -> TmFloat(_1.i, _1.v) )
# 694 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Support.Error.withinfo) in
    Obj.repr(
# 285 "parser.mly"
      ( fun ctx -> TmString(_1.i, _1.v) )
# 701 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int Support.Error.withinfo) in
    Obj.repr(
# 288 "parser.mly"
      ( fun ctx ->
          let rec f n = match n with
              0 -> TmZero(_1.i)
            | n -> TmSucc(_1.i, f (n-1))
          in f _1.v )
# 712 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 296 "parser.mly"
      ( fun ctx i -> [] )
# 718 "parser.ml"
               : 'Fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'NEFields) in
    Obj.repr(
# 299 "parser.mly"
      ( _1 )
# 725 "parser.ml"
               : 'Fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Field) in
    Obj.repr(
# 305 "parser.mly"
      ( fun ctx i -> [_1 ctx i] )
# 732 "parser.ml"
               : 'NEFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Field) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'NEFields) in
    Obj.repr(
# 308 "parser.mly"
      ( fun ctx i -> (_1 ctx i) :: (_3 ctx (i+1)) )
# 741 "parser.ml"
               : 'NEFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 313 "parser.mly"
      ( fun ctx i -> (_1.v, _3 ctx) )
# 750 "parser.ml"
               : 'Field))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 316 "parser.mly"
      ( fun ctx i -> (string_of_int i, _1 ctx) )
# 757 "parser.ml"
               : 'Field))
(* Entry toplevel *)
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
let toplevel (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf :  Syntax.context -> (Syntax.command list * Syntax.context) )
