# 16 "lexer.mll"
 
open Support.Error

  (* reservedWords are special words or symbols with functional meaning, they can't be used as variable names *)
let reservedWords = [
  (* Keywords *)
  ("if", fun i -> Parser.IF i);
  ("then", fun i -> Parser.THEN i);
  ("else", fun i -> Parser.ELSE i);
  ("true", fun i -> Parser.TRUE i);
  ("false", fun i -> Parser.FALSE i);
  ("Bool", fun i -> Parser.BOOL i);
  ("lambda", fun i -> Parser.LAMBDA i);
  ("timesfloat", fun i -> Parser.TIMESFLOAT i);
  ("succ", fun i -> Parser.SUCC i);
  ("pred", fun i -> Parser.PRED i);
  ("iszero", fun i -> Parser.ISZERO i);
  ("Nat", fun i -> Parser.NAT i);
  ("let", fun i -> Parser.LET i);
  ("in", fun i -> Parser.IN i);

  (* Symbols *)
  ("_", fun i -> Parser.USCORE i);
  ("'", fun i -> Parser.APOSTROPHE i);
  ("\"", fun i -> Parser.DQUOTE i);
  ("!", fun i -> Parser.BANG i);
  ("#", fun i -> Parser.HASH i);
  ("$", fun i -> Parser.TRIANGLE i);
  ("*", fun i -> Parser.STAR i);
  ("|", fun i -> Parser.VBAR i);
  (".", fun i -> Parser.DOT i);
  (";", fun i -> Parser.SEMI i);
  (",", fun i -> Parser.COMMA i);
  ("/", fun i -> Parser.SLASH i);
  (":", fun i -> Parser.COLON i);
  ("::", fun i -> Parser.COLONCOLON i);
  ("=", fun i -> Parser.EQ i);
  ("==", fun i -> Parser.EQEQ i);
  ("[", fun i -> Parser.LSQUARE i);
  ("<", fun i -> Parser.LT i);
  ("{", fun i -> Parser.LCURLY i);
  ("(", fun i -> Parser.LPAREN i);
  ("<-", fun i -> Parser.LEFTARROW i);
  ("{|", fun i -> Parser.LCURLYBAR i);
  ("[|", fun i -> Parser.LSQUAREBAR i);
  ("}", fun i -> Parser.RCURLY i);
  (")", fun i -> Parser.RPAREN i);
  ("]", fun i -> Parser.RSQUARE i);
  (">", fun i -> Parser.GT i);
  ("|}", fun i -> Parser.BARRCURLY i);
  ("|>", fun i -> Parser.BARGT i);
  ("|]", fun i -> Parser.BARRSQUARE i);

  (* Special compound symbols: *)
  (":=", fun i -> Parser.COLONEQ i);
  ("->", fun i -> Parser.ARROW i);
  ("=>", fun i -> Parser.DARROW i);
  ("==>", fun i -> Parser.DDARROW i);
]

(* Support functions *)


type buildfun = info -> Parser.token

(* This creates a hashtable for the reservedWords *)
let (symbolTable : (string,buildfun) Hashtbl.t) = Hashtbl.create 1024

(* All reservedWords are added to the hashtable *)
let _ =
  List.iter (fun (str,f) -> Hashtbl.add symbolTable str f) reservedWords

(* Tries to find str in the hashtable *)
(* If it finds it, it is returned as a token with its info *)
(* if it can't find it, it creates a new upper or lowercase ID (a variable) with location info and the value of str *)
let createID i str =
  try (Hashtbl.find symbolTable str) i
  with _ ->
    if (String.get str 0) >= 'A' && (String.get str 0) <= 'Z' then
       Parser.UCID {i=i;v=str}
    else
       Parser.LCID {i=i;v=str}

(* Initialisation of all variables *)

(* Lineno is the line number *)
let lineno   = ref 1
(* Depth accounts for nested comment depth *)
and depth    = ref 0
(* Start stores first character location value *)
and start    = ref 0
(* This stores the name and location of the file being parsed in a reference *)
and filename = ref ""
(* Startlex stores information on where lexbuf was located when entering any comment or string literal *)
and startLex = ref dummyinfo

(* create opens a file either by its name or its location + name *)
let create inFile stream =
  if not (Filename.is_implicit inFile) then filename := inFile
  else filename := Filename.concat (Sys.getcwd()) inFile;
  lineno := 1; start := 0; Lexing.from_channel stream

(* Every new line, this increments the linenumber and sets start to the first character in that line *)
let newline lexbuf = incr lineno; start := (Lexing.lexeme_start lexbuf)

(* When location info should be appended, this quotes the filename, line number and location *)
let info lexbuf =
  createInfo (!filename) (!lineno) (Lexing.lexeme_start lexbuf - !start)

(* Returns the current matched string *)
let text = Lexing.lexeme

(* Stores the string being parsed with literal string rules *)
let stringBuffer = ref (String.create 2048)

(* Tracks where the string contained in stringbuffer ends *)
let stringEnd = ref 0

(* Resets string end back to the beginning *)
let resetStr () = stringEnd := 0

(* addStr adds a new character to the stringBuffer and increments stringEnd *)
(* This is used when parsing literal strings *)
(* If stringBuffer is too small, a newBuffer of twice the size is created and used instead as stringBuffer *)
let addStr ch =
  let x = !stringEnd in
  let buffer = !stringBuffer
in
  if x = String.length buffer then
    begin
      let newBuffer = String.create (x*2) in
      String.blit buffer 0 newBuffer 0 x;
      String.set newBuffer x ch;
      stringBuffer := newBuffer;
      stringEnd := x+1
    end
  else
    begin
      String.set buffer x ch;
      stringEnd := x+1
    end

(* getStr returns the string contained in the buffer using stringEnd as length *)
let getStr () = String.sub (!stringBuffer) 0 (!stringEnd)

(* Extracts a line number from text, used with "# " and "# line" *)
let extractLineno yytext offset =
  int_of_string (String.sub yytext offset (String.length yytext - offset))

# 152 "lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\241\255\242\255\243\255\244\255\091\000\006\000\101\000\
    \007\000\071\000\115\000\090\000\185\000\218\000\039\001\108\000\
    \099\000\095\000\254\255\001\000\157\000\004\000\253\255\252\255\
    \049\001\038\000\059\001\035\000\046\000\118\000\069\001\079\001\
    \089\001\099\001\246\255\092\000\158\000\251\255\252\255\253\255\
    \109\000\115\000\255\255\254\255\130\000\255\255\161\000\162\000\
    \127\000\005\000\168\000\251\255\252\255\253\255\254\255\255\255\
    \125\001\249\255\135\001\251\255\252\255\253\255\254\255\255\255\
    \145\001\250\255";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\255\255\255\255\010\000\011\000\010\000\
    \011\000\011\000\010\000\011\000\010\000\008\000\006\000\011\000\
    \011\000\011\000\255\255\014\000\000\000\255\255\255\255\255\255\
    \255\255\255\255\004\000\255\255\255\255\255\255\255\255\005\000\
    \255\255\007\000\255\255\009\000\255\255\255\255\255\255\255\255\
    \003\000\003\000\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\006\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255";
  Lexing.lex_default = 
   "\001\000\000\000\000\000\000\000\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\255\255\255\255\000\000\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\038\000\000\000\000\000\000\000\
    \255\255\255\255\000\000\000\000\255\255\000\000\047\000\047\000\
    \255\255\049\000\051\000\000\000\000\000\000\000\000\000\000\000\
    \057\000\000\000\255\255\000\000\000\000\000\000\000\000\000\000\
    \255\255\000\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\020\000\018\000\018\000\020\000\019\000\018\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \020\000\004\000\003\000\015\000\005\000\005\000\005\000\004\000\
    \004\000\004\000\017\000\005\000\004\000\010\000\004\000\016\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\012\000\004\000\011\000\009\000\004\000\004\000\
    \005\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\006\000\005\000\004\000\004\000\013\000\
    \005\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\008\000\007\000\004\000\005\000\005\000\
    \005\000\005\000\034\000\034\000\035\000\034\000\005\000\034\000\
    \005\000\005\000\005\000\005\000\024\000\023\000\022\000\027\000\
    \005\000\028\000\005\000\029\000\034\000\005\000\030\000\005\000\
    \005\000\005\000\034\000\005\000\043\000\042\000\005\000\005\000\
    \005\000\049\000\044\000\034\000\045\000\005\000\020\000\018\000\
    \037\000\020\000\021\000\255\255\255\255\005\000\000\000\000\000\
    \000\000\034\000\053\000\005\000\000\000\000\000\000\000\005\000\
    \000\000\000\000\000\000\005\000\000\000\020\000\000\000\000\000\
    \000\000\005\000\034\000\255\255\255\255\005\000\000\000\000\000\
    \040\000\000\000\055\000\000\000\000\000\041\000\000\000\005\000\
    \000\000\000\000\000\000\005\000\000\000\000\000\034\000\005\000\
    \000\000\005\000\000\000\000\000\000\000\005\000\005\000\005\000\
    \000\000\005\000\034\000\005\000\005\000\000\000\005\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\005\000\
    \000\000\005\000\000\000\005\000\000\000\000\000\034\000\000\000\
    \000\000\005\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \002\000\013\000\000\000\000\000\054\000\255\255\000\000\000\000\
    \000\000\000\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\000\000\005\000\000\000\000\000\
    \000\000\005\000\000\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\005\000\000\000\005\000\
    \000\000\013\000\000\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\032\000\000\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\025\000\039\000\060\000\
    \000\000\255\255\255\255\000\000\059\000\000\000\000\000\000\000\
    \052\000\000\000\000\000\000\000\000\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\065\000\065\000\065\000\065\000\065\000\065\000\065\000\
    \065\000\065\000\065\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\061\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\063\000\000\000\000\000\000\000\000\000\
    \000\000\062\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\019\000\000\000\000\000\021\000\049\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\005\000\
    \005\000\005\000\006\000\008\000\009\000\009\000\005\000\011\000\
    \005\000\007\000\007\000\007\000\015\000\016\000\017\000\025\000\
    \007\000\027\000\007\000\028\000\011\000\005\000\029\000\010\000\
    \010\000\010\000\035\000\005\000\040\000\041\000\010\000\007\000\
    \010\000\048\000\044\000\007\000\044\000\007\000\020\000\020\000\
    \036\000\020\000\020\000\046\000\047\000\010\000\255\255\255\255\
    \255\255\010\000\050\000\010\000\255\255\255\255\255\255\005\000\
    \255\255\255\255\255\255\005\000\255\255\020\000\255\255\255\255\
    \255\255\007\000\007\000\046\000\047\000\007\000\255\255\255\255\
    \036\000\255\255\050\000\255\255\255\255\036\000\255\255\010\000\
    \255\255\255\255\255\255\010\000\255\255\255\255\011\000\005\000\
    \255\255\005\000\255\255\255\255\255\255\012\000\012\000\012\000\
    \255\255\007\000\007\000\007\000\012\000\255\255\012\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\010\000\
    \255\255\010\000\255\255\012\000\255\255\255\255\012\000\255\255\
    \255\255\012\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\013\000\255\255\255\255\050\000\049\000\255\255\255\255\
    \255\255\255\255\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\255\255\012\000\255\255\255\255\
    \255\255\012\000\255\255\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\012\000\255\255\012\000\
    \255\255\013\000\255\255\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\014\000\255\255\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\024\000\036\000\056\000\
    \255\255\046\000\047\000\255\255\056\000\255\255\255\255\255\255\
    \050\000\255\255\255\255\255\255\255\255\056\000\056\000\056\000\
    \056\000\056\000\056\000\056\000\056\000\056\000\056\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\056\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\056\000\255\255\255\255\255\255\255\255\
    \255\255\056\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\056\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec main lexbuf =
    __ocaml_lex_main_rec lexbuf 0
and __ocaml_lex_main_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 171 "lexer.mll"
                           ( main lexbuf )
# 373 "lexer.ml"

  | 1 ->
# 173 "lexer.mll"
                                  ( newline lexbuf; main lexbuf )
# 378 "lexer.ml"

  | 2 ->
# 175 "lexer.mll"
       ( error (info lexbuf) "Unmatched end of comment" )
# 383 "lexer.ml"

  | 3 ->
# 177 "lexer.mll"
       ( depth := 1; startLex := info lexbuf; comment lexbuf; main lexbuf )
# 388 "lexer.ml"

  | 4 ->
# 180 "lexer.mll"
    ( lineno := extractLineno (text lexbuf) 2 - 1; getFile lexbuf )
# 393 "lexer.ml"

  | 5 ->
# 183 "lexer.mll"
    ( lineno := extractLineno (text lexbuf) 7 - 1; getFile lexbuf )
# 398 "lexer.ml"

  | 6 ->
# 186 "lexer.mll"
    ( Parser.INTV{i=info lexbuf; v=int_of_string (text lexbuf)} )
# 403 "lexer.ml"

  | 7 ->
# 189 "lexer.mll"
    ( Parser.FLOATV{i=info lexbuf; v=float_of_string (text lexbuf)} )
# 408 "lexer.ml"

  | 8 ->
# 193 "lexer.mll"
    ( createID (info lexbuf) (text lexbuf) )
# 413 "lexer.ml"

  | 9 ->
# 197 "lexer.mll"
    ( createID (info lexbuf) (text lexbuf) )
# 418 "lexer.ml"

  | 10 ->
# 200 "lexer.mll"
    ( createID (info lexbuf) (text lexbuf) )
# 423 "lexer.ml"

  | 11 ->
# 204 "lexer.mll"
    ( createID (info lexbuf) (text lexbuf) )
# 428 "lexer.ml"

  | 12 ->
# 206 "lexer.mll"
       ( resetStr(); startLex := info lexbuf; string lexbuf )
# 433 "lexer.ml"

  | 13 ->
# 208 "lexer.mll"
      ( Parser.EOF(info lexbuf) )
# 438 "lexer.ml"

  | 14 ->
# 210 "lexer.mll"
     ( error (info lexbuf) "Illegal character" )
# 443 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_main_rec lexbuf __ocaml_lex_state

and comment lexbuf =
    __ocaml_lex_comment_rec lexbuf 36
and __ocaml_lex_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 217 "lexer.mll"
    ( depth := succ !depth; comment lexbuf )
# 455 "lexer.ml"

  | 1 ->
# 221 "lexer.mll"
    ( depth := pred !depth; if !depth > 0 then comment lexbuf )
# 460 "lexer.ml"

  | 2 ->
# 224 "lexer.mll"
    ( error (!startLex) "Comment not terminated" )
# 465 "lexer.ml"

  | 3 ->
# 227 "lexer.mll"
    ( comment lexbuf )
# 470 "lexer.ml"

  | 4 ->
# 230 "lexer.mll"
    ( newline lexbuf; comment lexbuf )
# 475 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_comment_rec lexbuf __ocaml_lex_state

and getFile lexbuf =
    __ocaml_lex_getFile_rec lexbuf 44
and __ocaml_lex_getFile_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 235 "lexer.mll"
            ( getName lexbuf )
# 487 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_getFile_rec lexbuf __ocaml_lex_state

and getName lexbuf =
    __ocaml_lex_getName_rec lexbuf 46
and __ocaml_lex_getName_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 240 "lexer.mll"
                ( filename := (text lexbuf); finishName lexbuf )
# 499 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_getName_rec lexbuf __ocaml_lex_state

and finishName lexbuf =
    __ocaml_lex_finishName_rec lexbuf 48
and __ocaml_lex_finishName_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 244 "lexer.mll"
                ( main lexbuf )
# 511 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_finishName_rec lexbuf __ocaml_lex_state

and string lexbuf =
    __ocaml_lex_string_rec lexbuf 50
and __ocaml_lex_string_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 249 "lexer.mll"
       ( Parser.STRINGV {i = !startLex; v=getStr()} )
# 523 "lexer.ml"

  | 1 ->
# 251 "lexer.mll"
       ( addStr(escaped lexbuf); string lexbuf )
# 528 "lexer.ml"

  | 2 ->
# 253 "lexer.mll"
       ( addStr '\n'; newline lexbuf; string lexbuf )
# 533 "lexer.ml"

  | 3 ->
# 255 "lexer.mll"
       ( error (!startLex) "String not terminated" )
# 538 "lexer.ml"

  | 4 ->
# 257 "lexer.mll"
       ( addStr (Lexing.lexeme_char lexbuf 0); string lexbuf )
# 543 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_string_rec lexbuf __ocaml_lex_state

and escaped lexbuf =
    __ocaml_lex_escaped_rec lexbuf 56
and __ocaml_lex_escaped_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 262 "lexer.mll"
       ( '\n' )
# 555 "lexer.ml"

  | 1 ->
# 264 "lexer.mll"
       ( '\t' )
# 560 "lexer.ml"

  | 2 ->
# 266 "lexer.mll"
        ( '\\' )
# 565 "lexer.ml"

  | 3 ->
# 268 "lexer.mll"
         ( '\034'  )
# 570 "lexer.ml"

  | 4 ->
# 270 "lexer.mll"
        ( '\'' )
# 575 "lexer.ml"

  | 5 ->
# 273 "lexer.mll"
    (
      let x = int_of_string(text lexbuf) in
      if x > 255 then
	error (info lexbuf) "Illegal character constant"
      else
	Char.chr x
    )
# 586 "lexer.ml"

  | 6 ->
# 282 "lexer.mll"
    ( error (info lexbuf) "Illegal character constant" )
# 591 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_escaped_rec lexbuf __ocaml_lex_state

;;
