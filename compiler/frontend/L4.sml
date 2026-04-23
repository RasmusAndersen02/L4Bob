structure L4 =
struct

  (* types for abstract syntax for L4 *)

  type pos = int * int  (* position: line, column *)

  (* number×type or variable×type *)
  datatype decl = ConstD of string * string | VarD of string * string

  (* argument list *)
  type args = decl list

  datatype instr = Prim of string * args * args * args * pos
  	   	 | Call of string * args * args * args * pos
  	   	 | Uncall of string * args * args *args * pos

  (* labels and arguments *)
  type entry = string list * args * pos

  (* arguments and labels *)
  type exit = args * string list * pos

  type block = entry * instr list * exit

  type func = string * args * block list * pos

  type program = func list

  (* invert *)

  (* Note: inArgs should be outArgs and vice-versa *)
  fun invertInstr (Prim (prim, outArgs, roArgs, inArgs, pos)) =
        Prim (L4prim.invertPrim prim, inArgs, roArgs, outArgs, pos)
    | invertInstr (Call (fname, outArgs, roArgs, inArgs, pos)) =
        Uncall (fname, inArgs, roArgs, outArgs, pos)
    | invertInstr (Uncall (fname, outArgs, roArgs, inArgs, pos)) =
        Call (fname, inArgs, roArgs, outArgs, pos)

  fun b2e "Begin" = "End"
    | b2e l = l

  fun e2b "End" = "Begin"
    | e2b l = l

  fun invertEntry (labels, args, pos) = (args, List.map b2e labels, pos)

  fun invertExit (args, labels, pos) = (List.map e2b labels, args, pos)

  fun invertBlock (entry, instrs, exit) =
        (invertExit exit,
	 List.rev (List.map invertInstr instrs),
	 invertEntry entry)

  fun invertFun (fname, args, bs, pos) =
        (fname, args, List.map invertBlock bs, pos)

  (* convert to text *)

  fun showDecl (ConstD (v, t)) = v ^ ":" ^ t
    | showDecl (VarD (x, t)) = x ^ ":" ^ t

  fun showArgs args =
    String.concatWith " " (List.map showDecl args)

  fun showInstr (Prim (prim, inArgs, roArgs, outArgs, pos)) =
          showArgs inArgs ^ " |> " ^ prim ^ " " ^ showArgs roArgs ^
	  " |> " ^ showArgs outArgs ^ "\n"
    | showInstr (Call (fname, inArgs, roArgs, outArgs, pos)) =
          showArgs inArgs ^ " |> call " ^ fname ^ " " ^ showArgs roArgs ^
	  " |> " ^ showArgs outArgs ^ "\n"
    | showInstr (Uncall (fname, inArgs, roArgs, outArgs, pos)) =
          showArgs inArgs ^ " |> uncall " ^ fname ^ " " ^ showArgs roArgs ^
	  " |> " ^ showArgs outArgs ^ "\n"

  fun showEntry (labs, args, pos) =
    String.concatWith " " labs ^ " |> " ^ showArgs args ^ "\n"

  fun showExit (args, labs, pos) =
    showArgs args ^ " |> " ^ String.concatWith " " labs ^ "\n\n"

  fun showBlock (entry, inst, exit) =
    showEntry entry ^
    String.concat (List.map showInstr inst) ^
    showExit exit

  fun showFunc (fname, args, bs, pos) =
    "function " ^ fname ^ " " ^ showArgs args ^ "\n\n" ^
    String.concat (List.map showBlock bs)

  fun showPgm p = String.concat (List.map showFunc p)

end
