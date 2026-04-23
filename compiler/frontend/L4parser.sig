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

val Program :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> L4.program;
