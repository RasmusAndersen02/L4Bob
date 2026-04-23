(* primitive functions for L4. Should be extended *)
(* values are strings *)
(* NOT made for speed *)
structure L4prim =
struct

  exception Error of string * (int*int)
  exception Sub

  val primTable = (* types for primitive functions *)
    [("=",(["b"],["u16","u16"],["b"])),
     ("<",(["b"],["u16","u16"],["b"])),
     ("+",(["u16"],["u16"],["u16"])),
     ("-",(["u16"],["u16"],["u16"])),
     ("*",(["u16"],["u16"],["u16"])),
     ("/",(["u16"],["u16"],["u16"])),
     ("%",([],["u16","u16"],["u16"])),
     ("%1",(["u16"],["u16","u16"],[])),
     ("not",(["b"],[],["b"])),
     ("odd",(["b"],["u16"],["b"])),
     ("rol1",(["u16"],[],["u16"])),
     ("ror1",(["u16"],[],["u16"])),
     ("rol",(["u16"],["u16"],["u16"])),
     ("ror",(["u16"],["u16"],["u16"])),
     ("swap",(["a16","u16"],["u16"],["a16","u16"])),
     (* for test messages *)
     ("test",([],["u16"],[])),
     ("test2",([],["u16","u16"],[])),
     ("test3",([],["u16","u16","u16"],[]))
    ]

  (* inverse functions *)
  fun invertPrim "+" = "-"
    | invertPrim "-" = "+"
    | invertPrim "*" = "/"
    | invertPrim "/" = "*"
    | invertPrim "%" = "%1"
    | invertPrim "%1" = "%"
    | invertPrim "rol1" = "ror1"
    | invertPrim "ror1" = "rol1"
    | invertPrim "getsize" = "assertsize"
    | invertPrim "assertsize" = "getsize"
    | invertPrim p = p  (* default is self-inverse *)


  val u16M = "65536" (* 2¹⁶ *)
  val u16m = "65535" (* 2¹⁶-1 *)
  val u15M = "32768" (* 2¹⁵ *)

  (* helper functions on strings *)
  fun negate "0" = "1"
    | negate _ = "0"

  (* remove leading zeroes *)
  fun removeZeroes ds =
    case ds of
      [#"0"] => ds
    | (#"0" :: ds1) => removeZeroes ds1
    | _ => ds

  (* increment little endian digit list *)
  fun increment [] = [#"1"]
    | increment (#"9" :: xs) = #"0" :: increment xs
    | increment (x :: xs) = Char.succ x :: xs

  (* add little endian digits *)
  fun addDigits x y c =
    let
      val sum = Char.ord x + Char.ord y + c - 2*Char.ord #"0"
    in
      if sum > 9 then (Char.chr (sum - 10 + Char.ord #"0"), 1)
      else (Char.chr (sum + Char.ord #"0"), 0)
    end

  (* add little endian digit lists *)
  fun add1 [] y 0 = y
    | add1 [] y 1 = increment y
    | add1 x [] 0 = x
    | add1 x [] 1 = increment x
    | add1 (x1::xs) (y1::ys) c =
        let
	  val (z, c1) = addDigits x1 y1 c
	in
	  z :: add1 xs ys c1
	end
    | add1 x y c = add1 x y c (* shouldn't happen *)

  (* add number strings *)
  fun add x y =
    String.implode
      (List.rev (add1 (List.rev (String.explode x))
                      (List.rev (String.explode y))
		      0))

  (* decrement little endian digit list *)
  fun decrement [] = raise Sub
    | decrement (#"0" :: xs) = #"9" :: decrement xs
    | decrement (x :: xs) = Char.pred x :: xs

  (* subtract little endian digits *)
  fun subDigits x y b =
    let
      val sum = Char.ord x - Char.ord y - b
    in
      if sum < 0 then(Char.chr (sum + 10 + Char.ord #"0"), 1)
      else (Char.chr (sum + Char.ord #"0"), 0)
    end

  (* subtract little endian digit lists *)
  fun sub1 [] [] 0 = []
    | sub1 [] y b = raise Sub
    | sub1 x [] 0 = x
    | sub1 x [] 1 = decrement x
    | sub1 (x1::xs) (y1::ys) b =
        let
	  val (z, b1) = subDigits x1 y1 b
	in
	  z :: sub1 xs ys b1
	end
    | sub1 x y b = sub1 x y b (* shouldn't happen *)

  (* subtract number strings *)
  fun subtract x y =
    let
      val digits = (List.rev (sub1 (List.rev (String.explode x))
                             (List.rev (String.explode y))
	   	             0))
    in
      String.implode (removeZeroes digits)
    end

  (* compare number strings, adding leading zeroes to same length *)
  fun compare x y =
    if String.size x < String.size y
    then compare ("0" ^ x) y
    else if String.size y < String.size x
    then compare x ("0" ^ y)
    else String.compare (x, y)

  (* if number >= 2¹⁶, then subtract 2¹⁶ *)
  fun rollOver x =
    case compare x u16M of
      LESS => x
    | _    => subtract x u16M

  fun odd x = 
        let
	  val lastDigit = Char.ord (List.last (String.explode x))
	in
	  lastDigit mod 2 = 1
	end

  fun div2 x =
    let
      fun div2' [] b = []
        | div2' (d :: ds) b =
	    if d mod 2 = 0
	    then d div 2 + b :: div2' ds 0
	    else if b=0 then d div 2 :: div2' ds 5
	    else d div 2 + b :: div2' ds 5
      val digits0 = List.map (fn d => Char.ord d - Char.ord #"0")
                             (String.explode x)
      val digits1 = div2' digits0 0
      val digits2 = List.map (fn d => Char.chr (d + Char.ord #"0")) digits1
    in
      String.implode (removeZeroes digits2)
    end

  fun div4 x = div2 (div2 x)
  fun div16 x = div4 (div4 x)
  fun div64 x = div16 (div16 x)

  fun times2 x = add x x
  fun times4 x = times2 (times2 x)
  fun times16 x = times4 (times4 x)
  fun times64 x = times16 (times16 x)

  fun mod16 x = subtract x (times16 (div16 x))

  fun rol1 x =
    let
      val x2 = add x x
    in
      case compare x2 u16M of
        LESS => x2
      | _ => subtract x2 u16m
    end

  fun rol x 0 = x
    | rol x n = rol (rol1 x) (n - 1)

  fun ror1 x =
    if odd x then add (div2 x) u15M
    else div2 x
    
  fun ror x 0 = x
    | ror x n = ror (ror1 x) (n - 1)

  fun even x =
    let
      val lastDigit = String.sub (x, String.size x - 1)
    in
      Char.ord lastDigit mod 2 = 0
    end

  fun mul "1" y = y
    | mul x y =
        if even x then mul (div2 x) (add y y)
        else (add y (mul (div2 (subtract x "1")) (add y y)))

  fun divi "0" y pos = "0"
    | divi x "0" pos = raise Error ("Division by 0", pos)
    | divi x "1" pos = x
    | divi x y pos =
        if x = y then "1"
	else (* hope numbers aren't too large *)
	  Int.toString (Option.getOpt (Int.fromString x, 0) div
	                Option.getOpt (Int.fromString y, 1))

  fun modu x y =
    Int.toString (Option.getOpt (Int.fromString x, 0) mod
                  Option.getOpt (Int.fromString y, 1))

  (* apply primitive function *)
  fun applyPrim name inValues roValues pos =
    case (name, inValues, roValues) of
      ("=", [b], [x,y]) =>
        (case compare x y of
	   EQUAL => [negate b]
	 | _ => [b])
    | ("<", [b], [x,y]) =>
        (case compare x y of
	   LESS => [negate b]
	 | _ => [b])
    | ("+", [x], [y]) => [rollOver (add x y)]
    | ("-", [x], [y]) =>
        ((case compare x y of
            LESS   => [subtract (add x u16M) y]
	  | EQUAL   => ["0"]
	  | GREATER => [subtract x y])
	 handle Sub => raise Error ("negative result", pos))
    | ("*" , [x], [y]) => [mul x y]
    | ("/" , [x], [y]) => [divi x y pos]
    | ("%" , [], [x,y]) => [modu x y]
    | ("%1" , [m], [x,y]) =>
        (case compare m (modu x y) of
	   EQUAL => []
	 | _ => raise Error ("modulus does not match", pos))
    | ("not", [b], []) => [negate b]
    | ("odd", [b], [x]) => if odd x then [negate b] else [b]
    | ("rol1", [x], []) => [rol1 x]
    | ("ror1", [x], []) => [ror1 x]
    | ("rol", [x], [r]) =>
        let
	  val r1 = mod16 r
	in
	  [rol x (Option.getOpt (Int.fromString r1, 0))]
	end
    | ("ror", [x], [r]) =>
        let
	  val r1 = mod16 r
	in
	  [ror x (Option.getOpt (Int.fromString r1, 0))]
	end
    | ("swap", [a,v], [i]) =>
        let
	  val aa = String.tokens Char.isSpace a
	  val s = List.length aa
	  val ii = Option.getOpt (Int.fromString i, 0)
	in
	  if ii >= s then raise  Error ("index " ^ i ^ " out of range", pos)
	  else if ii = 0 then
	    [String.concatWith " " (v :: List.drop (aa, 1)), hd aa]
	  else
	    [String.concatWith " "
	      (List.take (aa, ii) @ [v] @ List.drop (aa, ii+1)),
	     List.nth (aa, ii)]
	end
    (* the following three instructions generate test messages *)
    | ("test", [], [v]) =>
        (TextIO.output (TextIO.stdErr, v ^ "\n"); [])
    | ("test2", [], [v,w]) =>
        (TextIO.output (TextIO.stdErr, v ^" "^ w ^"\n"); [])
    | ("test3", [], [v,w,z]) =>
        (TextIO.output (TextIO.stdErr, v ^" "^ w ^" "^ z ^"\n"); [])
    | (name, inArgs, roArgs) =>
         raise Error (name ^ " not found with this number of arguments", pos)

(* rename applyPrim above to applyPrim1 when uncommenting this
  fun applyPrim name inValues roValues pos =
    (TextIO.output(TextIO.stdErr,
      String.concatWith " " inValues ^" → " ^ name ^ " " ^ String.concatWith " " roValues ^" → ");
      let
        val v = applyPrim1 name inValues roValues pos
      in
        (TextIO.output(TextIO.stdErr, String.concatWith " " v ^ "\n");
         v)
      end)
*)

end
