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

fun show_graph_entry ((variable, neighbors) : L4utils.Var * L4utils.VarSet) : string =
  variable ^ " -> " ^ show_varset neighbors

fun print_graph (graph : L4interference.IGraph) : unit =
  let
    val entries = Binarymap.listItems graph
  in
    List.app (fn entry => print (show_graph_entry entry ^ "\n")) entries
  end

fun run_for_function (function_name : string, blocks : L4.block list) : unit =
  let
    val block_map = L4utils.map_id_to_block blocks
    val cfg = L4cfg.build_cfg blocks
    val edge_args = L4cfg.build_edge_args (block_map, cfg)
    val liveness = L4liveness.analyze (blocks, cfg, edge_args)
    val graph = L4interference.build_instr_mapping (#instr_live liveness)
    val _ = print ("Function: " ^ function_name ^ "\n")
  in
    print_graph graph
  end

fun run_on_program (program : L4.program) : unit =
  List.app (fn (function_name, _, blocks, _) => run_for_function (function_name, blocks)) program

val _ =
  case CommandLine.arguments () of
    [path] => run_on_program (parse_program_from_file path)
  | _ => print "Usage: interference_test <path-to-l4-file>\n"
