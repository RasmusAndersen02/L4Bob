(* type check L4 program *)
structure L4type =
struct

  exception Error of string * (int * int)

  (* find name in table, raise error if not found *)
  fun lookup x [] pos = raise Error (x ^ " not found1", pos)
    | lookup x ((y,v) :: table) pos =
         if x=y then v else lookup x table pos

  (* find name in table, return option type *)
  fun lookup1 x [] pos = NONE
    | lookup1 x ((y,v) :: table) pos =
         if x=y then SOME v else lookup1 x table pos

  (* find any name from 1st argument in table, raise error if not found *)
  fun lookupList [] table pos = raise Error ("no id found", pos)
    | lookupList (x::xs) table pos =
        case lookup1 x table pos of
	  SOME v => v
	| NONE => lookupList xs table pos

  (* remove first occurence of name from table, raise error if not found *)
  fun remove x [] pos = raise Error (x ^ " not found2", pos)
    | remove x ((y,v) :: table) pos =
         if x=y then table else (y,v) :: remove x table pos

  fun isIn x xs = List.exists (fn y => y = x) xs

  (* Add declared variables to list of available variables *)
  fun addArgs [] vars roVars pos = ([], vars)
    | addArgs (L4.ConstD (_, t) :: args) vars roVars pos =
        let
	  val (ts, vars1) = addArgs args vars roVars pos
	in
	  (t :: ts, vars1)
	end
    | addArgs (L4.VarD (x, t) :: args) vars roVars pos =
        let
	  val (ts, vars2) = addArgs args vars roVars pos
	in
	  if isIn x (List.map (fn (x,t) => x) roVars) then
	  raise Error ("read-only var used as parameter", pos)
	  else (t :: ts, (x, t) :: vars2)
	end

  (* Remove declared variables from list of available variables *)
  fun dropArgs [] vars roVars pos = ([], vars)
    | dropArgs (L4.ConstD (_, t) :: args) vars roVars pos =
        let
	  val (ts, vars1) = dropArgs args vars roVars pos
	in
	  (t :: ts, vars1)
	end
    | dropArgs (L4.VarD (x, t) :: args) vars roVars pos =
        let
	  val t1 = lookup x vars pos
	  val vars1 = remove x vars pos
	  val (ts, vars2) = dropArgs args vars1 roVars pos
	in
	  if isIn x (List.map (fn (x,t) => x) roVars) then
	  raise Error ("read-only var used as parameter", pos)
	  else if t = t1 then (t :: ts, vars2)
	  else raise Error (x ^ "has inconsistent type", pos)
	end

  fun hasRepeats [] = false
    | hasRepeats (L4.ConstD _ :: xs) = hasRepeats xs
    | hasRepeats ((xt as L4.VarD (x,t)) :: xs) =
         isIn xt xs orelse hasRepeats xs

  (* Check type correctness of instruction and update variable list *)
  fun checkInst i vars roVars primTable funTable =
    case i of
      L4.Prim (p, inArgs, roArgs, outArgs, pos) =>
        let
	  val (inT, roT, outT) = lookup p primTable pos
	  val (inT1, vars1) = dropArgs inArgs vars roVars pos
	  val (roT1, _) = dropArgs roArgs (vars1 @ roVars) [] pos
	  val (outT1, vars2) = addArgs outArgs vars1 roVars pos
        in
	  if inT <> inT1 then
	    raise Error ("intypes don't match: " ^ String.concatWith " " inT ^ " / " ^ String.concatWith " " inT1, pos)
	  else if roT <> roT1 then
	    raise Error ("read-only types don't match", pos)
	  else if outT <> outT1 then
	    raise Error ("outtypes don't match", pos)
	  else if hasRepeats (inArgs @ roArgs @ outArgs) then
	    raise Error ("repeated arguments", pos)
	  else vars2
	end
    | L4.Call (f, inArgs, roArgs, outArgs, pos) =>
        let
	  val (inT, roT, outT) = lookup f funTable pos
	  val (inT1, vars1) = dropArgs inArgs vars roVars pos
	  val (roT1, _) = dropArgs roArgs (vars1 @ roVars) [] pos
	  val (outT1, vars2) = addArgs outArgs vars1 roVars pos
        in
	  if inT <> inT1 then
	    raise Error ("intypes don't match: " ^ String.concatWith ";" inT ^" versus " ^ String.concatWith ";" inT1, pos)
	  else if roT <> roT1 then
	    raise Error ("read-only types don't match", pos)
	  else if outT <> outT1 then
	    raise Error ("outtypes don't match", pos)
	  else vars2
	end
    | L4.Uncall (f, inArgs, roArgs, outArgs, pos) =>
        let
	  val (inT, roT, outT) = lookup f funTable pos
	  val (outT1, vars1) = dropArgs inArgs vars roVars pos
	  val (roT1, _) = dropArgs roArgs (vars1 @ roVars) [] pos
	  val (inT1, vars2) = addArgs outArgs vars1 roVars pos
        in
	  if inT <> inT1 then
	    raise Error ("intypes don't match: " ^ String.concatWith ";" inT ^" versus " ^ String.concatWith ";" inT1, pos)
	  else if roT <> roT1 then
	    raise Error ("read-only types don't match", pos)
	  else if outT <> outT1 then
	    raise Error ("outtypes don't match", pos)
	  else vars2
	end

  (* Check sequence of instructions and update variable list *)
  fun checkInsts ins vars roVars primTable funTable =
    case ins of
      [] => vars
    | (i::ins) =>
        let
	  val vars2 = checkInst i vars roVars primTable funTable
	in
	  checkInsts ins vars2 roVars primTable funTable
	end

  (* Update label environment given list of labels and argument types *)
  (* and position of labels *)
  fun updateLabelTable [] argTypesFreeVars labelEnv pos = labelEnv
    | updateLabelTable (l1 :: ls) argTypesFreeVars labelEnv pos =
        case lookup1 l1 labelEnv pos of
	  SOME l1TypesVars => (* already in table, check consistency *)
	    if l1TypesVars = argTypesFreeVars
	    then updateLabelTable ls argTypesFreeVars labelEnv pos
	    else raise Error ("types of label " ^ l1 ^ " do no match use", pos)
        | NONE => (* not in table, add to table *)
	    updateLabelTable ls argTypesFreeVars
	                     ((l1,argTypesFreeVars) :: labelEnv) pos

  (* Check block given argument types and return updated label environment*)
  (* Limitation: constants not checked to be of declared type
  *)
  fun checkBlock
         argTypes
         ((inLabels, args1, pos1), ins, (args2, outLabels, pos2))
         freeVars roVars labelEnv primTable funTable =
    let
      val (args1Types, freeVars1) = addArgs args1 freeVars roVars pos1
    in
      if argTypes <> (tl args1Types) then
        raise Error ("Label arguments don't match declaration", pos1)
      else
        let
	  val freeVars2 = checkInsts ins freeVars1 roVars primTable funTable
          val (args2Types, freeVars3) = dropArgs args2 freeVars2 roVars pos2
	in
	  if isIn "End" outLabels andalso freeVars3 <> [] then
	    raise Error ("Free variables at End of block", pos2)
	  else
	    updateLabelTable outLabels (tl args2Types, freeVars3) labelEnv pos2
	end
    end

  (* look up block by entry label in list of blocks *)
  fun lookupBlock lab [] = raise Error ("label " ^ lab ^ " not found3", (0,0))
    | lookupBlock lab ((b as ((labs1, _, _), _, _)) :: bs) =
        if isIn lab labs1 then b else lookupBlock lab bs

  (* check work list of labels and update label environment *)
  fun checkLabels [] labelEnv bs roVars primTable funTable = labelEnv
    | checkLabels ("End" :: labs) labelEnv bs roVars primTable funTable =
        checkLabels labs labelEnv bs roVars primTable funTable
    | checkLabels (lab :: labs) labelEnv bs roVars primTable funTable =
        let
	  val (block as ((inLabels, args1, pos1),ins,(args2, outLabels, pos2)))
	       = lookupBlock lab bs
	  val (argsTypes, freeVars) = lookup lab labelEnv pos1
	  val (args1Types, _) = addArgs args1 [] roVars pos1
          val labelEnv1 = checkBlock argsTypes block freeVars roVars
		                     labelEnv primTable funTable
	in
	  if tl args1Types = argsTypes andalso labelEnv1 = labelEnv then
	    checkLabels labs labelEnv1 bs roVars primTable funTable
	  else
	    checkLabels (labs @ outLabels)
		        labelEnv1 bs roVars primTable funTable 
	end

  (* Check blocks in function.  Uses worklist algorithm *)
  fun checkBlocks bs beginArgs roVars primTable funTable pos =
    let
      val labelEnv = [("Begin", (beginArgs,[]))] (* no free variables *)
    in
      checkLabels ["Begin"] labelEnv bs roVars primTable funTable
    end

  (* check function declaration *)
  fun checkFun (fname, funArgs, bs, pos) primTable funTable =
    let
      val (roTypes, roVars) = addArgs funArgs [] [] pos
      val  ((inLabels, args1, pos1),ins,(args2, outLabels, pos2)) =
              lookupBlock "Begin" bs
      val (beginTypes, beginVars) =  addArgs (tl args1) [] roVars pos
      val labelEnv = checkBlocks bs beginTypes roVars primTable funTable pos
    in
      ()
    end

  fun makeFunTable [] = []
    | makeFunTable ((fname, funArgs, bs, pos) :: fs) =
        let
	  val funTable = makeFunTable fs
	  val (roTypes, roVars) = addArgs funArgs [] [] pos
	  val ((labs1, args1, _), _, _) = lookupBlock "Begin" bs
          val (beginTypes, beginVars) =  addArgs (tl args1) [] roVars pos
	  val ((labs1, args2, _), _, _) =
	        lookupBlock "Begin" (List.map L4.invertBlock bs)
          val (endTypes, endVars) =  addArgs (tl args2) [] roVars pos
	in
	  (fname, (beginTypes, roTypes, endTypes)) :: funTable
	end

  val funTable = (* should be generated from program *)
    [("fib2",(["u16"],[],["u16","u16"]))]
    
  fun check fs =
    let val fTable = makeFunTable fs in
      List.app (fn f => checkFun f L4prim.primTable fTable) fs
    end

end
