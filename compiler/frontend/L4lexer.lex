{
 open Lexing;

 exception LexicalError of string * (int * int) (* (message, (line, column)) *)

 val currentLine = ref 1
 val lineStartPos = ref [0]

 fun getPos lexbuf = getLineCol (getLexemeStart lexbuf)
				(!currentLine)
				(!lineStartPos)

 and getLineCol pos line (p1::ps) =
       if pos>=p1 then (line, pos-p1)
       else getLineCol pos (line-1) ps
   | getLineCol pos line [] = raise LexicalError ("",(0,0))

 fun lexerError lexbuf s = 
     raise LexicalError (s, getPos lexbuf)

 fun keyword (s, pos) =
     case s of
         "call"         => L4parser.CALL pos
       | "uncall"       => L4parser.UNCALL pos
       | "function"     => L4parser.FUNCTION pos
       | _              => L4parser.ID (s, pos)

 }

rule Token = parse
    [` ` `\t`]+         { Token lexbuf } (* whitespace *)
  | "//" [^`\n`]*	{ Token lexbuf } (* comment *)
  | `\n`                { currentLine := !currentLine+1;
                          lineStartPos :=  getLexemeStart lexbuf
			                   :: !lineStartPos;
                          L4parser.NEWLINE (getPos lexbuf)} (* newline *)
  | `:`                 { L4parser.COLON (getPos lexbuf) }
  | "|>"                { L4parser.PIPE (getPos lexbuf) }
  | [`0`-`9`]+          { L4parser.NUM (getLexeme lexbuf, getPos lexbuf) }
  | [`A`-`Z`][`!`-`&` `*`-`9` `<`-`{` `}`-`~`]*
                        { L4parser.LABEL (getLexeme lexbuf, getPos lexbuf) }
  | [`!`-`&` `*`-`9` `<`-`{` `}`-`~`]+
                        { keyword (getLexeme lexbuf,getPos lexbuf) }
  | eof                 { L4parser.EOF (getPos lexbuf) }
  | _                   { lexerError lexbuf "Illegal symbol in input" }
 
;
