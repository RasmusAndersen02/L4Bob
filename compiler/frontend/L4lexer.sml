local open Obj Lexing in


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

 
fun action_9 lexbuf = (
 lexerError lexbuf "Illegal symbol in input" )
and action_8 lexbuf = (
 L4parser.EOF (getPos lexbuf) )
and action_7 lexbuf = (
 keyword (getLexeme lexbuf,getPos lexbuf) )
and action_6 lexbuf = (
 L4parser.LABEL (getLexeme lexbuf, getPos lexbuf) )
and action_5 lexbuf = (
 L4parser.NUM (getLexeme lexbuf, getPos lexbuf) )
and action_4 lexbuf = (
 L4parser.PIPE (getPos lexbuf) )
and action_3 lexbuf = (
 L4parser.COLON (getPos lexbuf) )
and action_2 lexbuf = (
 currentLine := !currentLine+1;
                          lineStartPos :=  getLexemeStart lexbuf
			                   :: !lineStartPos;
                          L4parser.NEWLINE (getPos lexbuf))
and action_1 lexbuf = (
 Token lexbuf )
and action_0 lexbuf = (
 Token lexbuf )
and state_0 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"!" andalso currChar <= #"&" then  state_5 lexbuf
 else if currChar >= #"[" andalso currChar <= #"{" then  state_5 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_9 lexbuf
 else if currChar >= #"0" andalso currChar <= #"9" then  state_7 lexbuf
 else case currChar of
    #"." => state_5 lexbuf
 |  #"-" => state_5 lexbuf
 |  #"," => state_5 lexbuf
 |  #"+" => state_5 lexbuf
 |  #"*" => state_5 lexbuf
 |  #"@" => state_5 lexbuf
 |  #"?" => state_5 lexbuf
 |  #">" => state_5 lexbuf
 |  #"=" => state_5 lexbuf
 |  #"<" => state_5 lexbuf
 |  #"~" => state_5 lexbuf
 |  #"}" => state_5 lexbuf
 |  #"\t" => state_3 lexbuf
 |  #" " => state_3 lexbuf
 |  #"|" => state_10 lexbuf
 |  #":" => action_3 lexbuf
 |  #"/" => state_6 lexbuf
 |  #"\n" => action_2 lexbuf
 |  #"\^@" => action_8 lexbuf
 |  _ => action_9 lexbuf
 end)
and state_3 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_0);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => state_17 lexbuf
 |  #" " => state_17 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_5 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_7);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"!" andalso currChar <= #"&" then  state_13 lexbuf
 else if currChar >= #"*" andalso currChar <= #"9" then  state_13 lexbuf
 else if currChar >= #"<" andalso currChar <= #"{" then  state_13 lexbuf
 else case currChar of
    #"~" => state_13 lexbuf
 |  #"}" => state_13 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_6 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_7);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"!" andalso currChar <= #"&" then  state_13 lexbuf
 else if currChar >= #"0" andalso currChar <= #"9" then  state_13 lexbuf
 else if currChar >= #"<" andalso currChar <= #"{" then  state_13 lexbuf
 else case currChar of
    #"." => state_13 lexbuf
 |  #"-" => state_13 lexbuf
 |  #"," => state_13 lexbuf
 |  #"+" => state_13 lexbuf
 |  #"*" => state_13 lexbuf
 |  #"~" => state_13 lexbuf
 |  #"}" => state_13 lexbuf
 |  #"/" => state_15 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_7 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_5);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"!" andalso currChar <= #"&" then  state_13 lexbuf
 else if currChar >= #"*" andalso currChar <= #"/" then  state_13 lexbuf
 else if currChar >= #"<" andalso currChar <= #"{" then  state_13 lexbuf
 else if currChar >= #"0" andalso currChar <= #"9" then  state_14 lexbuf
 else case currChar of
    #"~" => state_13 lexbuf
 |  #"}" => state_13 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_9 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_6);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"!" andalso currChar <= #"&" then  state_12 lexbuf
 else if currChar >= #"*" andalso currChar <= #"9" then  state_12 lexbuf
 else if currChar >= #"<" andalso currChar <= #"{" then  state_12 lexbuf
 else case currChar of
    #"~" => state_12 lexbuf
 |  #"}" => state_12 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_10 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_9);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #">" => action_4 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_12 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_6);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"!" andalso currChar <= #"&" then  state_12 lexbuf
 else if currChar >= #"*" andalso currChar <= #"9" then  state_12 lexbuf
 else if currChar >= #"<" andalso currChar <= #"{" then  state_12 lexbuf
 else case currChar of
    #"~" => state_12 lexbuf
 |  #"}" => state_12 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_13 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_7);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"!" andalso currChar <= #"&" then  state_13 lexbuf
 else if currChar >= #"*" andalso currChar <= #"9" then  state_13 lexbuf
 else if currChar >= #"<" andalso currChar <= #"{" then  state_13 lexbuf
 else case currChar of
    #"~" => state_13 lexbuf
 |  #"}" => state_13 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_14 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_5);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"!" andalso currChar <= #"&" then  state_13 lexbuf
 else if currChar >= #"*" andalso currChar <= #"/" then  state_13 lexbuf
 else if currChar >= #"<" andalso currChar <= #"{" then  state_13 lexbuf
 else if currChar >= #"0" andalso currChar <= #"9" then  state_14 lexbuf
 else case currChar of
    #"~" => state_13 lexbuf
 |  #"}" => state_13 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_15 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_1);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"!" andalso currChar <= #"&" then  state_15 lexbuf
 else if currChar >= #"*" andalso currChar <= #"9" then  state_15 lexbuf
 else if currChar >= #"<" andalso currChar <= #"{" then  state_15 lexbuf
 else case currChar of
    #"~" => state_15 lexbuf
 |  #"}" => state_15 lexbuf
 |  #"\^@" => backtrack lexbuf
 |  #"\n" => backtrack lexbuf
 |  _ => state_16 lexbuf
 end)
and state_16 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_1);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\^@" => backtrack lexbuf
 |  #"\n" => backtrack lexbuf
 |  _ => state_16 lexbuf
 end)
and state_17 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_0);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => state_17 lexbuf
 |  #" " => state_17 lexbuf
 |  _ => backtrack lexbuf
 end)
and Token lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_0 lexbuf)

(* The following checks type consistency of actions *)
val _ = fn _ => [action_9, action_8, action_7, action_6, action_5, action_4, action_3, action_2, action_1, action_0];

end
