structure L4check =
struct
  (* check for consistency:
       - functions have different names
       - parameters have different names
       - variables defined and consumed exactly once each
       - Begin and End occur in one entry and exit, respectively
       - other labels occur in one entry and one exit each
   *)

  exception Error of string * (int * int)

  fun remove x [] = []
    | remove x (y::ys) = if x=y then remove x ys else y :: remove x ys

  fun qsort [] = []
    | qsort [x : string] = [x]
    | qsort (x::xs) = qsort (List.filter (fn y => y<x) xs) @
                      (x :: qsort (List.filter (fn y => y>x) xs))

  fun checkDuplicates [] kind pos = ()
    | checkDuplicates (f::fs) kind pos =
        if List.exists (fn y => y=f) fs
	then raise Error ("dublicate " ^ kind ^ ": " ^ f, pos)
	else checkDuplicates fs kind pos

  fun getAnames [] = []
    | getAnames (L4.ConstD _ :: ds) = getAnames ds
    | getAnames (L4.VarD (x,t) :: ds) = x :: getAnames ds

  fun checkInstr (L4.Prim (name, outArgs, roArgs, inArgs, pos)) =
        checkDuplicates (getAnames (outArgs @ roArgs @ inArgs)) "argument" pos
    | checkInstr (L4.Call (name, outArgs, roArgs, inArgs, pos)) =
        checkDuplicates (getAnames (outArgs @ roArgs @ inArgs)) "argument" pos
    | checkInstr (L4.Uncall (name, outArgs, roArgs, inArgs, pos)) =
        checkDuplicates (getAnames (outArgs @ roArgs @ inArgs)) "argument" pos

  fun checkBlock ((labels1, args1, pos1), instrs, (args2, labels2, pos2)) =
    let
      val () = checkDuplicates (getAnames args1) "argument" pos1
      val () = checkDuplicates (getAnames args2) "argument" pos2
    in
      List.app checkInstr instrs
    end


  fun checkFunction (name, args, blocks : L4.block list, pos) =
      let
        val () = checkDuplicates (getAnames args) "argument" pos
        val entryLabels = List.concat (List.map (#1 o #1) blocks)
	val exitLabels = List.concat (List.map (#2 o #3) blocks)
        val () = checkDuplicates entryLabels "entry label" pos
        val () = checkDuplicates exitLabels "exit label" pos
      in
	if not (List.exists (fn l => l = "Begin") entryLabels)
	then raise Error ("no Begin label", pos)
	else if not (List.exists (fn l => l = "End") exitLabels)
	then raise Error ("no End label", pos)
	else
	  let
	    val entryLabels1 = remove "Begin" entryLabels
	    val exitLabels1 = remove "End" exitLabels
	  in
	    if qsort entryLabels1 = qsort exitLabels1
	    then List.app checkBlock blocks
	    else raise Error
	                ("Bad labels: " ^
			 String.concatWith ";" entryLabels1 ^ " != " ^
			 String.concatWith ";" exitLabels1, pos)
	  end
      end

  fun check (fs : L4.program) =
    (checkDuplicates (List.map (#1) fs) "function name" (0,0);
     List.app checkFunction fs)

end
