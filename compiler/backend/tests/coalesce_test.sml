fun parse_program_from_file (path : string) : L4.program =
  let
    val input_stream = Nonstdio.open_in_bin path
    val lexbuf = Lexing.createLexer (fn buffer => fn requested => Nonstdio.buff_input input_stream buffer 0 requested)
    val program = L4parser.Program L4lexer.Token lexbuf
    val _ = BasicIO.close_in input_stream
  in
    program
  end

fun show_preference ((source_var, target_var) : L4interference.Preference) : string =
  "(" ^ source_var ^ ", " ^ target_var ^ ")"

fun print_preferences (preferences : L4interference.Preference list) : unit =
  List.app (fn preference => print (show_preference preference ^ "\n")) preferences

fun run_for_function (function_name : string, blocks : L4.block list) : unit =
  let
    val block_map = L4cfg.block_id_mapping blocks
    val cfg = L4cfg.build_cfg blocks
    val edge_args = L4cfg.build_edge_args (block_map, cfg)
    val preferences = L4interference.coalesce_boundaries edge_args
    val _ = print ("Function: " ^ function_name ^ "\n")
  in
    print_preferences preferences
  end

fun run_on_program (program : L4.program) : unit =
  List.app (fn (function_name, _, blocks, _) => run_for_function (function_name, blocks)) program

val _ =
  case CommandLine.arguments () of
    [path] => run_on_program (parse_program_from_file path)
  | _ => print "Usage: coalesce_test <path-to-l4-file>\n"
