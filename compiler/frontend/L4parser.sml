local
type t__1__ = (int*int)
type t__2__ = (int*int)
type t__3__ = (int*int)
type t__4__ = (int*int)
type t__5__ = string*(int*int)
type t__6__ = string*(int*int)
type t__7__ = (int*int)
type t__8__ = string*(int*int)
type t__9__ = (int*int)
type t__10__ = (int*int)
in
datatype token =
    CALL of t__1__
  | COLON of t__2__
  | EOF of t__3__
  | FUNCTION of t__4__
  | ID of t__5__
  | LABEL of t__6__
  | NEWLINE of t__7__
  | NUM of t__8__
  | PIPE of t__9__
  | UNCALL of t__10__
end;

open Obj Parsing;
prim_val vector_ : int -> 'a -> 'a Vector.vector = 2 "make_vect";
prim_val update_ : 'a Vector.vector -> int -> 'a -> unit = 3 "set_vect_item";


(* Line 7, file frontend/L4parser.sml *)
val yytransl = #[
  257 (* CALL *),
  258 (* COLON *),
  259 (* EOF *),
  260 (* FUNCTION *),
  261 (* ID *),
  262 (* LABEL *),
  263 (* NEWLINE *),
  264 (* NUM *),
  265 (* PIPE *),
  266 (* UNCALL *),
    0];

val yylhs = "\255\255\
\\002\000\002\000\003\000\003\000\001\000\004\000\005\000\005\000\
\\006\000\012\000\012\000\007\000\008\000\008\000\008\000\009\000\
\\009\000\010\000\010\000\011\000\011\000\011\000\011\000\013\000\
\\013\000\000\000";

val yylen = "\002\000\
\\005\000\006\000\001\000\002\000\002\000\003\000\001\000\002\000\
\\004\000\001\000\002\000\004\000\007\000\008\000\008\000\000\000\
\\002\000\000\000\002\000\003\000\003\000\003\000\003\000\001\000\
\\002\000\002\000";

val yydefred = "\000\000\
\\000\000\000\000\000\000\000\000\026\000\000\000\000\000\000\000\
\\000\000\025\000\004\000\005\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\019\000\000\000\021\000\
\\023\000\020\000\022\000\000\000\000\000\001\000\016\000\000\000\
\\000\000\011\000\008\000\000\000\000\000\002\000\006\000\017\000\
\\000\000\000\000\000\000\009\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\012\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\013\000\000\000\014\000\015\000";

val yydgoto = "\002\000\
\\005\000\006\000\007\000\029\000\030\000\031\000\039\000\040\000\
\\036\000\016\000\017\000\032\000\008\000";

val yysindex = "\005\000\
\\011\255\000\000\040\255\039\255\000\000\011\255\044\255\045\255\
\\012\255\000\000\000\000\000\000\043\255\048\255\049\255\039\255\
\\012\255\012\255\022\255\027\255\046\255\000\000\039\255\000\000\
\\000\000\000\000\000\000\046\255\046\255\000\000\000\000\047\255\
\\046\255\000\000\000\000\012\255\012\255\000\000\000\000\000\000\
\\050\255\039\255\003\255\000\000\053\255\012\255\055\255\039\255\
\\012\255\052\255\012\255\000\000\054\255\012\255\056\255\012\255\
\\039\255\012\255\039\255\000\000\039\255\000\000\000\000";

val yyrindex = "\000\000\
\\000\000\000\000\000\000\020\255\000\000\051\255\000\000\000\000\
\\057\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\254\254\057\255\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\033\255\007\255\000\000\000\000\000\000\
\\000\000\000\000\000\000\058\255\057\255\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\058\255\000\000\000\000\
\\058\255\000\000\058\255\000\000\000\000\057\255\000\000\057\255\
\\000\000\057\255\000\000\000\000\000\000\000\000\000\000";

val yygindex = "\000\000\
\\000\000\000\000\056\000\000\000\004\000\000\000\000\000\000\000\
\\000\000\241\255\000\000\229\255\252\255";

val YYTABLESIZE = 67;
val yytable = "\010\000\
\\034\000\022\000\023\000\045\000\018\000\001\000\018\000\046\000\
\\028\000\007\000\007\000\021\000\047\000\007\000\003\000\048\000\
\\014\000\004\000\033\000\015\000\041\000\042\000\024\000\024\000\
\\024\000\024\000\024\000\024\000\024\000\025\000\050\000\026\000\
\\035\000\053\000\027\000\055\000\038\000\044\000\057\000\010\000\
\\059\000\010\000\061\000\052\000\009\000\004\000\012\000\018\000\
\\013\000\019\000\020\000\028\000\060\000\003\000\062\000\037\000\
\\063\000\049\000\043\000\051\000\054\000\011\000\056\000\018\000\
\\058\000\000\000\018\000";

val yycheck = "\004\000\
\\028\000\017\000\018\000\001\001\007\001\001\000\009\001\005\001\
\\006\001\003\001\004\001\016\000\010\001\007\001\004\001\043\000\
\\005\001\007\001\023\000\008\001\036\000\037\000\003\001\004\001\
\\005\001\006\001\005\001\008\001\009\001\008\001\046\000\005\001\
\\029\000\049\000\008\001\051\000\033\000\042\000\054\000\007\001\
\\056\000\009\001\058\000\048\000\005\001\007\001\003\001\005\001\
\\004\001\002\001\002\001\006\001\057\000\003\001\059\000\009\001\
\\061\000\005\001\009\001\005\001\009\001\006\000\009\001\007\001\
\\009\001\255\255\009\001";

val yyact = vector_ 27 (fn () => ((raise Fail "parser") : obj));
(* Rule 1, file frontend/L4parser.grm, line 28 *)
val _ = update_ yyact 1
(fn () => repr(let
val d__1__ = peekVal 4 : (int*int)
val d__2__ = peekVal 3 : string*(int*int)
val d__3__ = peekVal 2 : L4.args
val d__4__ = peekVal 1 : unit
val d__5__ = peekVal 0 : L4.block list
in
( (#1 (d__2__), (d__3__), (d__5__), (d__1__)) ) end : L4.func))
;
(* Rule 2, file frontend/L4parser.grm, line 30 *)
val _ = update_ yyact 2
(fn () => repr(let
val d__1__ = peekVal 5 : unit
val d__2__ = peekVal 4 : (int*int)
val d__3__ = peekVal 3 : string*(int*int)
val d__4__ = peekVal 2 : L4.args
val d__5__ = peekVal 1 : unit
val d__6__ = peekVal 0 : L4.block list
in
( (#1 (d__3__), (d__4__), (d__6__), (d__2__)) ) end : L4.func))
;
(* Rule 3, file frontend/L4parser.grm, line 33 *)
val _ = update_ yyact 3
(fn () => repr(let
val d__1__ = peekVal 0 : L4.func
in
( [(d__1__)] ) end : L4.func list))
;
(* Rule 4, file frontend/L4parser.grm, line 34 *)
val _ = update_ yyact 4
(fn () => repr(let
val d__1__ = peekVal 1 : L4.func
val d__2__ = peekVal 0 : L4.func list
in
( (d__1__) :: (d__2__) ) end : L4.func list))
;
(* Rule 5, file frontend/L4parser.grm, line 37 *)
val _ = update_ yyact 5
(fn () => repr(let
val d__1__ = peekVal 1 : L4.func list
val d__2__ = peekVal 0 : (int*int)
in
( (d__1__) ) end : L4.program))
;
(* Rule 6, file frontend/L4parser.grm, line 41 *)
val _ = update_ yyact 6
(fn () => repr(let
val d__1__ = peekVal 2 : L4.entry
val d__2__ = peekVal 1 : L4.instr list
val d__3__ = peekVal 0 : L4.exit
in
( ((d__1__), (d__2__), (d__3__)) ) end : L4.block))
;
(* Rule 7, file frontend/L4parser.grm, line 44 *)
val _ = update_ yyact 7
(fn () => repr(let
val d__1__ = peekVal 0 : L4.block
in
( [(d__1__)] ) end : L4.block list))
;
(* Rule 8, file frontend/L4parser.grm, line 45 *)
val _ = update_ yyact 8
(fn () => repr(let
val d__1__ = peekVal 1 : L4.block
val d__2__ = peekVal 0 : L4.block list
in
( (d__1__) :: (d__2__) ) end : L4.block list))
;
(* Rule 9, file frontend/L4parser.grm, line 49 *)
val _ = update_ yyact 9
(fn () => repr(let
val d__1__ = peekVal 3 : string list
val d__2__ = peekVal 2 : (int*int)
val d__3__ = peekVal 1 : L4.args
val d__4__ = peekVal 0 : unit
in
( ((d__1__), (d__3__), (d__2__)) ) end : L4.entry))
;
(* Rule 10, file frontend/L4parser.grm, line 52 *)
val _ = update_ yyact 10
(fn () => repr(let
val d__1__ = peekVal 0 : string*(int*int)
in
( [#1 (d__1__)] ) end : string list))
;
(* Rule 11, file frontend/L4parser.grm, line 53 *)
val _ = update_ yyact 11
(fn () => repr(let
val d__1__ = peekVal 1 : string*(int*int)
val d__2__ = peekVal 0 : string list
in
( #1 (d__1__) :: (d__2__) ) end : string list))
;
(* Rule 12, file frontend/L4parser.grm, line 57 *)
val _ = update_ yyact 12
(fn () => repr(let
val d__1__ = peekVal 3 : L4.args
val d__2__ = peekVal 2 : (int*int)
val d__3__ = peekVal 1 : string list
val d__4__ = peekVal 0 : unit
in
( ((d__1__), (d__3__), (d__2__)) ) end : L4.exit))
;
(* Rule 13, file frontend/L4parser.grm, line 61 *)
val _ = update_ yyact 13
(fn () => repr(let
val d__1__ = peekVal 6 : L4.args
val d__2__ = peekVal 5 : (int*int)
val d__3__ = peekVal 4 : string*(int*int)
val d__4__ = peekVal 3 : L4.args
val d__5__ = peekVal 2 : (int*int)
val d__6__ = peekVal 1 : L4.args
val d__7__ = peekVal 0 : unit
in
( L4.Prim (#1 (d__3__), (d__1__), (d__4__), (d__6__), (d__2__)) ) end : L4.instr))
;
(* Rule 14, file frontend/L4parser.grm, line 63 *)
val _ = update_ yyact 14
(fn () => repr(let
val d__1__ = peekVal 7 : L4.args
val d__2__ = peekVal 6 : (int*int)
val d__3__ = peekVal 5 : (int*int)
val d__4__ = peekVal 4 : string*(int*int)
val d__5__ = peekVal 3 : L4.args
val d__6__ = peekVal 2 : (int*int)
val d__7__ = peekVal 1 : L4.args
val d__8__ = peekVal 0 : unit
in
( L4.Call (#1 (d__4__), (d__1__), (d__5__), (d__7__), (d__2__)) ) end : L4.instr))
;
(* Rule 15, file frontend/L4parser.grm, line 65 *)
val _ = update_ yyact 15
(fn () => repr(let
val d__1__ = peekVal 7 : L4.args
val d__2__ = peekVal 6 : (int*int)
val d__3__ = peekVal 5 : (int*int)
val d__4__ = peekVal 4 : string*(int*int)
val d__5__ = peekVal 3 : L4.args
val d__6__ = peekVal 2 : (int*int)
val d__7__ = peekVal 1 : L4.args
val d__8__ = peekVal 0 : unit
in
( L4.Uncall (#1 (d__4__), (d__1__), (d__5__), (d__7__), (d__2__)) ) end : L4.instr))
;
(* Rule 16, file frontend/L4parser.grm, line 68 *)
val _ = update_ yyact 16
(fn () => repr(let
in
( [] ) end : L4.instr list))
;
(* Rule 17, file frontend/L4parser.grm, line 69 *)
val _ = update_ yyact 17
(fn () => repr(let
val d__1__ = peekVal 1 : L4.instr list
val d__2__ = peekVal 0 : L4.instr
in
( (d__1__) @  [(d__2__)] ) end : L4.instr list))
;
(* Rule 18, file frontend/L4parser.grm, line 72 *)
val _ = update_ yyact 18
(fn () => repr(let
in
( [] ) end : L4.args))
;
(* Rule 19, file frontend/L4parser.grm, line 73 *)
val _ = update_ yyact 19
(fn () => repr(let
val d__1__ = peekVal 1 : L4.decl
val d__2__ = peekVal 0 : L4.args
in
( (d__1__) :: (d__2__) ) end : L4.args))
;
(* Rule 20, file frontend/L4parser.grm, line 76 *)
val _ = update_ yyact 20
(fn () => repr(let
val d__1__ = peekVal 2 : string*(int*int)
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : string*(int*int)
in
( L4.ConstD (#1 (d__1__), #1 (d__3__)) ) end : L4.decl))
;
(* Rule 21, file frontend/L4parser.grm, line 77 *)
val _ = update_ yyact 21
(fn () => repr(let
val d__1__ = peekVal 2 : string*(int*int)
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : string*(int*int)
in
( L4.VarD (#1 (d__1__), #1 (d__3__)) ) end : L4.decl))
;
(* Rule 22, file frontend/L4parser.grm, line 78 *)
val _ = update_ yyact 22
(fn () => repr(let
val d__1__ = peekVal 2 : string*(int*int)
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : string*(int*int)
in
( L4.ConstD (#1 (d__1__), #1 (d__3__)) ) end : L4.decl))
;
(* Rule 23, file frontend/L4parser.grm, line 79 *)
val _ = update_ yyact 23
(fn () => repr(let
val d__1__ = peekVal 2 : string*(int*int)
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : string*(int*int)
in
( L4.VarD (#1 (d__1__), #1 (d__3__)) ) end : L4.decl))
;
(* Rule 24, file frontend/L4parser.grm, line 82 *)
val _ = update_ yyact 24
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( () ) end : unit))
;
(* Rule 25, file frontend/L4parser.grm, line 84 *)
val _ = update_ yyact 25
(fn () => repr(let
val d__1__ = peekVal 1 : (int*int)
val d__2__ = peekVal 0 : unit
in
( () ) end : unit))
;
(* Entry Program *)
val _ = update_ yyact 26 (fn () => raise yyexit (peekVal 0));
val yytables : parseTables =
  ( yyact,
    yytransl,
    yylhs,
    yylen,
    yydefred,
    yydgoto,
    yysindex,
    yyrindex,
    yygindex,
    YYTABLESIZE,
    yytable,
    yycheck );
fun Program lexer lexbuf = yyparse yytables 1 lexer lexbuf;
