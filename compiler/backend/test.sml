type ID = L4liveness.ID
type LiveSet = L4liveness.LiveSet
type UseDefSet = L4liveness.UseDefSet
type InstrInfo = LiveSet * UseDefSet

fun parse_program_from_file path =
  let
    val ins = Nonstdio.open_in_bin path
    val lexbuf = Lexing.createLexer (fn buf => fn n => Nonstdio.buff_input ins buf 0 n)
    val program = L4parser.Program L4lexer.Token lexbuf
    val _ = BasicIO.close_in ins
  in
    program
  end

fun show_varset vars =
  let
    val names = Binaryset.listItems vars
  in
    "{" ^ String.concatWith ", " names ^ "}"
  end

fun show_liveset ({live_in, live_out} : LiveSet) =
  "in=" ^ show_varset live_in ^ " out=" ^ show_varset live_out

fun show_usedef ({use, def} : UseDefSet) =
  "use=" ^ show_varset use ^ " def=" ^ show_varset def

fun show_instr_key (block_id : ID, instr_id : ID) =
  "(" ^ Int.toString block_id ^ "," ^ Int.toString instr_id ^ ")"

fun print_within_block_liveness_dict live_dict =
  let
    val items = Binarymap.listItems live_dict
    fun print_item ((key, (live_set, usedef_set)) : (ID * ID) * InstrInfo) =
      print
        (show_instr_key key ^ " -> " ^
         show_liveset live_set ^ " " ^
         show_usedef usedef_set ^ "\n")
  in
    List.app print_item items
  end

fun print_within_block_liveness_map_from_block_map block_map =
  let
    val live_dict = L4liveness.build_within_block_liveness_map block_map
  in
    print_within_block_liveness_dict live_dict
  end

fun print_within_block_liveness_map blocks =
  let
    val block_map = L4cfg.block_id_mapping blocks
  in
    print_within_block_liveness_map_from_block_map block_map
  end

fun run_on_file path =
  let
    val program = parse_program_from_file path
    val _ = L4check.check program
    val _ =
      case program of
        [] => print "No functions in input program\n"
      | (fname, _, blocks, _) :: _ =>
          (print ("Function: " ^ fname ^ "\n");
           print_within_block_liveness_map blocks)
  in
    ()
  end

val _ =
  case CommandLine.arguments () of
    [path] => run_on_file path
  | _ =>
      print "Usage: backend/test <path-to-l4-file>\n"
