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
# 66 "parser.ml"
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
  270 (* IN *);
  271 (* UCID *);
  272 (* LCID *);
  273 (* INTV *);
  274 (* FLOATV *);
  275 (* STRINGV *);
  276 (* APOSTROPHE *);
  277 (* DQUOTE *);
  278 (* ARROW *);
  279 (* BANG *);
  280 (* BARGT *);
  281 (* BARRCURLY *);
  282 (* BARRSQUARE *);
  283 (* COLON *);
  284 (* COLONCOLON *);
  285 (* COLONEQ *);
  286 (* COLONHASH *);
  287 (* COMMA *);
  288 (* DARROW *);
  289 (* DDARROW *);
  290 (* DOT *);
    0 (* EOF *);
  291 (* EQ *);
  292 (* EQEQ *);
  293 (* EXISTS *);
  294 (* GT *);
  295 (* HASH *);
  296 (* LCURLY *);
  297 (* LCURLYBAR *);
  298 (* LEFTARROW *);
  299 (* LPAREN *);
  300 (* LSQUARE *);
  301 (* LSQUAREBAR *);
  302 (* LT *);
  303 (* RCURLY *);
  304 (* RPAREN *);
  305 (* RSQUARE *);
  306 (* SEMI *);
  307 (* SLASH *);
  308 (* STAR *);
  309 (* TRIANGLE *);
  310 (* USCORE *);
  311 (* VBAR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\004\000\004\000\004\000\005\000\
\007\000\007\000\007\000\006\000\006\000\008\000\008\000\003\000\
\003\000\003\000\003\000\003\000\003\000\009\000\009\000\009\000\
\009\000\009\000\009\000\010\000\010\000\010\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\012\000\012\000\
\013\000\013\000\014\000\014\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\002\000\001\000\002\000\002\000\001\000\
\003\000\001\000\001\000\003\000\001\000\000\000\002\000\001\000\
\006\000\006\000\006\000\006\000\006\000\001\000\002\000\003\000\
\002\000\002\000\002\000\003\000\003\000\001\000\003\000\001\000\
\001\000\001\000\003\000\001\000\001\000\001\000\000\000\001\000\
\001\000\003\000\003\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\032\000\033\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\038\000\036\000\037\000\001\000\
\000\000\000\000\045\000\000\000\003\000\000\000\000\000\030\000\
\034\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\005\000\004\000\000\000\044\000\
\000\000\040\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\010\000\011\000\000\000\
\007\000\008\000\000\000\006\000\000\000\035\000\000\000\031\000\
\002\000\028\000\029\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\043\000\042\000\000\000\000\000\000\000\000\000\
\000\000\009\000\012\000\017\000\018\000\019\000\020\000\021\000"

let yydgoto = "\002\000\
\019\000\020\000\021\000\038\000\057\000\058\000\059\000\000\000\
\022\000\023\000\024\000\041\000\042\000\043\000"

let yysindex = "\009\000\
\001\000\000\000\020\000\000\000\000\000\244\254\216\255\216\255\
\216\255\216\255\245\254\032\255\000\000\000\000\000\000\000\000\
\048\000\020\000\000\000\234\254\000\000\216\255\251\254\000\000\
\000\000\029\255\007\255\010\255\064\000\251\254\251\254\251\254\
\011\255\020\255\005\255\020\000\000\000\000\000\021\255\000\000\
\014\255\000\000\026\255\015\255\001\000\251\254\242\254\020\000\
\005\255\005\255\251\254\020\000\020\000\000\000\000\000\005\255\
\000\000\000\000\042\255\000\000\020\000\000\000\048\000\000\000\
\000\000\000\000\000\000\063\255\034\255\037\255\059\255\070\255\
\041\255\005\255\000\000\000\000\020\000\020\000\020\000\020\000\
\020\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\179\255\000\000\000\000\000\000\000\000\
\043\255\000\000\000\000\000\000\000\000\156\255\004\255\000\000\
\000\000\000\000\000\000\000\000\000\000\022\255\077\255\083\255\
\000\000\000\000\000\000\000\000\000\000\000\000\207\255\000\000\
\000\000\000\000\045\255\000\000\000\000\132\255\000\000\000\000\
\000\000\000\000\138\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\238\254\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\053\000\000\000\253\255\000\000\219\255\029\000\000\000\000\000\
\000\000\208\000\000\000\000\000\041\000\000\000"

let yytablesize = 363
let yytable = "\026\000\
\016\000\066\000\067\000\027\000\033\000\022\000\022\000\022\000\
\022\000\001\000\054\000\069\000\070\000\040\000\044\000\013\000\
\055\000\022\000\073\000\022\000\022\000\022\000\022\000\025\000\
\025\000\025\000\025\000\045\000\047\000\013\000\048\000\013\000\
\060\000\049\000\022\000\025\000\050\000\025\000\025\000\025\000\
\025\000\028\000\034\000\022\000\068\000\052\000\022\000\056\000\
\071\000\072\000\022\000\022\000\025\000\022\000\053\000\061\000\
\063\000\075\000\035\000\040\000\062\000\025\000\064\000\074\000\
\025\000\077\000\036\000\078\000\025\000\025\000\079\000\025\000\
\080\000\084\000\085\000\086\000\087\000\088\000\026\000\026\000\
\026\000\026\000\037\000\081\000\027\000\027\000\027\000\027\000\
\082\000\039\000\026\000\041\000\026\000\026\000\026\000\026\000\
\027\000\065\000\027\000\027\000\027\000\027\000\083\000\076\000\
\000\000\000\000\000\000\026\000\000\000\000\000\000\000\000\000\
\000\000\027\000\000\000\000\000\026\000\000\000\000\000\026\000\
\000\000\000\000\027\000\026\000\026\000\027\000\026\000\000\000\
\000\000\027\000\027\000\000\000\027\000\023\000\023\000\023\000\
\023\000\000\000\000\000\024\000\024\000\024\000\024\000\000\000\
\000\000\023\000\000\000\023\000\023\000\023\000\023\000\024\000\
\000\000\024\000\024\000\024\000\024\000\016\000\016\000\000\000\
\000\000\000\000\023\000\000\000\000\000\000\000\000\000\000\000\
\024\000\016\000\000\000\023\000\000\000\000\000\023\000\000\000\
\000\000\024\000\023\000\023\000\024\000\023\000\034\000\034\000\
\024\000\024\000\016\000\024\000\000\000\000\000\000\000\000\000\
\000\000\000\000\034\000\034\000\034\000\034\000\000\000\000\000\
\000\000\000\000\016\000\016\000\000\000\016\000\000\000\000\000\
\000\000\000\000\034\000\034\000\034\000\000\000\029\000\030\000\
\031\000\032\000\034\000\004\000\005\000\034\000\034\000\034\000\
\034\000\034\000\000\000\000\000\034\000\046\000\000\000\025\000\
\013\000\014\000\015\000\000\000\051\000\034\000\000\000\000\000\
\034\000\000\000\000\000\000\000\000\000\000\000\034\000\000\000\
\000\000\034\000\000\000\000\000\000\000\034\000\000\000\017\000\
\000\000\003\000\018\000\000\000\004\000\005\000\000\000\006\000\
\007\000\008\000\009\000\010\000\000\000\011\000\000\000\000\000\
\012\000\013\000\014\000\015\000\003\000\000\000\000\000\004\000\
\005\000\000\000\006\000\007\000\008\000\009\000\010\000\000\000\
\011\000\000\000\000\000\025\000\013\000\014\000\015\000\000\000\
\017\000\000\000\000\000\018\000\000\000\000\000\000\000\000\000\
\003\000\000\000\000\000\004\000\005\000\000\000\006\000\007\000\
\008\000\009\000\010\000\017\000\011\000\000\000\018\000\039\000\
\013\000\014\000\015\000\004\000\005\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\025\000\
\013\000\014\000\015\000\000\000\000\000\000\000\000\000\017\000\
\000\000\000\000\018\000\000\000\000\000\000\000\000\000\000\000\
\000\000\047\000\000\000\000\000\000\000\000\000\000\000\017\000\
\000\000\000\000\018\000"

let yycheck = "\003\000\
\000\000\016\001\017\001\016\001\016\001\002\001\003\001\004\001\
\005\001\001\000\006\001\049\000\050\000\017\000\018\000\034\001\
\012\001\014\001\056\000\016\001\017\001\018\001\019\001\002\001\
\003\001\004\001\005\001\050\001\034\001\048\001\002\001\050\001\
\036\000\027\001\031\001\014\001\027\001\016\001\017\001\018\001\
\019\001\054\001\054\001\040\001\048\000\035\001\043\001\043\001\
\052\000\053\000\047\001\048\001\031\001\050\001\035\001\035\001\
\031\001\061\000\027\001\063\000\047\001\040\001\048\001\022\001\
\043\001\003\001\035\001\034\001\047\001\048\001\034\001\050\001\
\014\001\077\000\078\000\079\000\080\000\081\000\002\001\003\001\
\004\001\005\001\051\001\014\001\002\001\003\001\004\001\005\001\
\048\001\047\001\014\001\047\001\016\001\017\001\018\001\019\001\
\014\001\045\000\016\001\017\001\018\001\019\001\074\000\063\000\
\255\255\255\255\255\255\031\001\255\255\255\255\255\255\255\255\
\255\255\031\001\255\255\255\255\040\001\255\255\255\255\043\001\
\255\255\255\255\040\001\047\001\048\001\043\001\050\001\255\255\
\255\255\047\001\048\001\255\255\050\001\002\001\003\001\004\001\
\005\001\255\255\255\255\002\001\003\001\004\001\005\001\255\255\
\255\255\014\001\255\255\016\001\017\001\018\001\019\001\014\001\
\255\255\016\001\017\001\018\001\019\001\002\001\003\001\255\255\
\255\255\255\255\031\001\255\255\255\255\255\255\255\255\255\255\
\031\001\014\001\255\255\040\001\255\255\255\255\043\001\255\255\
\255\255\040\001\047\001\048\001\043\001\050\001\004\001\005\001\
\047\001\048\001\031\001\050\001\255\255\255\255\255\255\255\255\
\255\255\255\255\016\001\017\001\018\001\019\001\255\255\255\255\
\255\255\255\255\047\001\048\001\255\255\050\001\255\255\255\255\
\255\255\255\255\004\001\005\001\034\001\255\255\007\000\008\000\
\009\000\010\000\040\001\004\001\005\001\043\001\016\001\017\001\
\018\001\019\001\255\255\255\255\050\001\022\000\255\255\016\001\
\017\001\018\001\019\001\255\255\029\000\031\001\255\255\255\255\
\034\001\255\255\255\255\255\255\255\255\255\255\040\001\255\255\
\255\255\043\001\255\255\255\255\255\255\047\001\255\255\040\001\
\255\255\001\001\043\001\255\255\004\001\005\001\255\255\007\001\
\008\001\009\001\010\001\011\001\255\255\013\001\255\255\255\255\
\016\001\017\001\018\001\019\001\001\001\255\255\255\255\004\001\
\005\001\255\255\007\001\008\001\009\001\010\001\011\001\255\255\
\013\001\255\255\255\255\016\001\017\001\018\001\019\001\255\255\
\040\001\255\255\255\255\043\001\255\255\255\255\255\255\255\255\
\001\001\255\255\255\255\004\001\005\001\255\255\007\001\008\001\
\009\001\010\001\011\001\040\001\013\001\255\255\043\001\016\001\
\017\001\018\001\019\001\004\001\005\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\016\001\
\017\001\018\001\019\001\255\255\255\255\255\255\255\255\040\001\
\255\255\255\255\043\001\255\255\255\255\255\255\255\255\255\255\
\255\255\034\001\255\255\255\255\255\255\255\255\255\255\040\001\
\255\255\255\255\043\001"

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
# 122 "parser.mly"
      ( fun ctx -> [],ctx )
# 358 "parser.ml"
               :  Syntax.context -> (Syntax.command list * Syntax.context) ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Command) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Syntax.context -> (Syntax.command list * Syntax.context) ) in
    Obj.repr(
# 127 "parser.mly"
      ( fun ctx ->
          let cmd,ctx = _1 ctx in
          let cmds,ctx = _3 ctx in
          cmd::cmds,ctx )
# 370 "parser.ml"
               :  Syntax.context -> (Syntax.command list * Syntax.context) ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 137 "parser.mly"
      ( fun ctx -> (let t = _1 ctx in Eval(tmInfo t,t)),ctx )
# 377 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Binder) in
    Obj.repr(
# 142 "parser.mly"
      ( fun ctx -> ((Bind(_1.i,_1.v,_2 ctx)), addname ctx _1.v) )
# 385 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 149 "parser.mly"
      ( fun ctx -> NameBind )
# 392 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 153 "parser.mly"
      ( fun ctx -> TmAbbBind(_2 ctx, None) )
# 400 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 155 "parser.mly"
    ( fun ctx -> VarBind (_2 ctx))
# 408 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ArrowType) in
    Obj.repr(
# 159 "parser.mly"
                ( _1 )
# 415 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 164 "parser.mly"
           ( _2 )
# 424 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 166 "parser.mly"
      ( fun ctx -> TyBool )
# 431 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 168 "parser.mly"
      ( fun ctx -> TyNat )
# 438 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'AType) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ArrowType) in
    Obj.repr(
# 175 "parser.mly"
           ( fun ctx -> TyArr(_1 ctx, _3 ctx) )
# 447 "parser.ml"
               : 'ArrowType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AType) in
    Obj.repr(
# 177 "parser.mly"
                  ( _1 )
# 454 "parser.ml"
               : 'ArrowType))
; (fun __caml_parser_env ->
    Obj.repr(
# 181 "parser.mly"
  ( fun ctx -> TyVarBind )
# 460 "parser.ml"
               : 'TyBinder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 183 "parser.mly"
  ( fun ctx -> TyAbbBind(_2 ctx) )
# 468 "parser.ml"
               : 'TyBinder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AppTerm) in
    Obj.repr(
# 189 "parser.mly"
      ( _1 )
# 475 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 193 "parser.mly"
      ( fun ctx -> TmIf(_1, _2 ctx, _4 ctx, _6 ctx) )
# 487 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Type) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 197 "parser.mly"
      ( fun ctx ->
          let ctx1 = addname ctx _2.v in
          TmAbs(_1, _2.v, _4 ctx, _6 ctx1) )
# 501 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Type) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 202 "parser.mly"
      ( fun ctx ->
          let ctx1 = addname ctx "_" in
          TmAbs(_1, "_", _4 ctx, _6 ctx1) )
# 515 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 208 "parser.mly"
      ( fun ctx -> TmLet(_1, _2.v, _4 ctx, _6 (addname ctx _2.v)) )
# 527 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 211 "parser.mly"
      ( fun ctx -> TmLet(_1, "_", _4 ctx, _6 (addname ctx "_")) )
# 539 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 216 "parser.mly"
      ( _1 )
# 546 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'AppTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 219 "parser.mly"
      ( fun ctx ->
          let e1 = _1 ctx in
          let e2 = _2 ctx in
          TmApp(tmInfo e1,e1,e2) )
# 557 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'PathTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 225 "parser.mly"
      ( fun ctx -> TmTimesfloat(_1, _2 ctx, _3 ctx) )
# 566 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 228 "parser.mly"
      ( fun ctx -> TmSucc(_1, _2 ctx) )
# 574 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 231 "parser.mly"
      ( fun ctx -> TmPred(_1, _2 ctx) )
# 582 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 234 "parser.mly"
      ( fun ctx -> TmIsZero(_1, _2 ctx) )
# 590 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'PathTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string Support.Error.withinfo) in
    Obj.repr(
# 240 "parser.mly"
      ( fun ctx ->
          TmProj(_2, _1 ctx, _3.v) )
# 600 "parser.ml"
               : 'PathTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'PathTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int Support.Error.withinfo) in
    Obj.repr(
# 245 "parser.mly"
      ( fun ctx ->
          TmProj(_2, _1 ctx, string_of_int _3.v) )
# 610 "parser.ml"
               : 'PathTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 249 "parser.mly"
      ( _1 )
# 617 "parser.ml"
               : 'PathTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 255 "parser.mly"
      ( _2 )
# 626 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 258 "parser.mly"
      ( fun ctx -> TmTrue(_1) )
# 633 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 261 "parser.mly"
      ( fun ctx -> TmFalse(_1) )
# 640 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Support.Error.withinfo) in
    Obj.repr(
# 265 "parser.mly"
      ( fun ctx ->
          TmVar(_1.i, name2index _1.i ctx _1.v, ctxlength ctx) )
# 648 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Fields) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 269 "parser.mly"
      ( fun ctx ->
          TmRecord(_1, _2 ctx 1) )
# 658 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float Support.Error.withinfo) in
    Obj.repr(
# 273 "parser.mly"
      ( fun ctx -> TmFloat(_1.i, _1.v) )
# 665 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Support.Error.withinfo) in
    Obj.repr(
# 276 "parser.mly"
      ( fun ctx -> TmString(_1.i, _1.v) )
# 672 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int Support.Error.withinfo) in
    Obj.repr(
# 279 "parser.mly"
      ( fun ctx ->
          let rec f n = match n with
              0 -> TmZero(_1.i)
            | n -> TmSucc(_1.i, f (n-1))
          in f _1.v )
# 683 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 287 "parser.mly"
      ( fun ctx i -> [] )
# 689 "parser.ml"
               : 'Fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'NEFields) in
    Obj.repr(
# 290 "parser.mly"
      ( _1 )
# 696 "parser.ml"
               : 'Fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Field) in
    Obj.repr(
# 296 "parser.mly"
      ( fun ctx i -> [_1 ctx i] )
# 703 "parser.ml"
               : 'NEFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Field) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'NEFields) in
    Obj.repr(
# 299 "parser.mly"
      ( fun ctx i -> (_1 ctx i) :: (_3 ctx (i+1)) )
# 712 "parser.ml"
               : 'NEFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 304 "parser.mly"
      ( fun ctx i -> (_1.v, _3 ctx) )
# 721 "parser.ml"
               : 'Field))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 307 "parser.mly"
      ( fun ctx i -> (string_of_int i, _1 ctx) )
# 728 "parser.ml"
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
