fun parse_program_from_file (path : string) : L4.program =
  let
    val input_stream = Nonstdio.open_in_bin path
    val lexbuf = Lexing.createLexer (fn buffer => fn requested => Nonstdio.buff_input input_stream buffer 0 requested)
    val program = L4parser.Program L4lexer.Token lexbuf
    val _ = BasicIO.close_in input_stream
  in
    program
  end

fun show_varset (variables : L4utils.VarSet) : string =
  let
    val names = Binaryset.listItems variables
  in
    "{" ^ String.concatWith ", " names ^ "}"
  end

fun show_tid ((block_id, instr_id) : L4utils.TID) : string =
  "(" ^ Int.toString block_id ^ "," ^ Int.toString instr_id ^ ")"

fun show_instr_compact (instr : L4.instr) : string =
  String.translate
    (fn #"\n" => " " | c => String.str c)
    (L4.showInstr instr)

fun print_instr_info ((tid, info) : L4utils.TID * L4utils.InstrInfo) : unit =
  let
    val ud = #ud info
    val lv = #lv info
  in
    print ("  Instr " ^ show_tid tid ^ "\n");
    print ("    code     : " ^ show_instr_compact (#instr info) ^ "\n");
    print ("    use      : " ^ show_varset (#uses ud) ^ "\n");
    print ("    def      : " ^ show_varset (#defs ud) ^ "\n");
    print ("    live_in  : " ^ show_varset (#live_in lv) ^ "\n");
    print ("    live_out : " ^ show_varset (#live_out lv) ^ "\n")
  end

fun run_for_function (function_name : string, blocks : L4.block list) : unit =
  let
    val block_map = L4cfg.block_id_mapping blocks
    val cfg = L4cfg.build_cfg blocks
    val edge_args = L4cfg.build_edge_args (block_map, cfg)
    val liveness = L4liveness.analyze (blocks, cfg, edge_args)
    val entries = Binarymap.listItems (#instr_live liveness)
  in
    print ("Function: " ^ function_name ^ "\n");
    List.app print_instr_info entries;
    print "\n"
  end

fun run_on_program (program : L4.program) : unit =
  List.app (fn (function_name, _, blocks, _) => run_for_function (function_name, blocks)) program

val _ =
  case CommandLine.arguments () of
    [path] => run_on_program (parse_program_from_file path)
  | _ => print "Usage: liveness_detail_test <path-to-l4-file>\n"
