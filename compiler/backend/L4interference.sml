structure L4interference :> L4interference =
struct
  type Var = L4utils.Var
  type VarSet = L4utils.VarSet
  type InstrInfo = L4utils.InstrInfo
  type TID = L4utils.TID

  type EdgeArgs = L4cfg.EdgeArgs
  type EdgeArgsMap = L4cfg.EdgeArgsMap

  type IGraph = (Var, VarSet) Binarymap.dict
  type Preference = Var * Var

  fun empty_graph () : IGraph = Binarymap.mkDict String.compare

  fun peek_value_or_init (graph : IGraph, key : Var) : VarSet =
    case Binarymap.peek (graph, key) of
      SOME interferences => interferences
    | NONE => L4utils.ini_varset ()

  fun peek_key_or_insert (graph : IGraph, key : Var) : IGraph =
    case Binarymap.peek (graph, key) of
      SOME _ => graph
    | NONE => Binarymap.insert (graph, key, L4utils.ini_varset ())

  fun add_undirected_edge 
    (graph : IGraph, vertex1 : Var, vertex2 : Var) 
    : IGraph =
    if vertex1 = vertex2 then 
      peek_key_or_insert (graph, vertex1)
    else
      let
        val graph_with_keys =
          peek_key_or_insert (peek_key_or_insert (graph, vertex1), vertex2)
        val inter_vertex1 = peek_value_or_init (graph_with_keys, vertex1)
        val inter_vertex2 = peek_value_or_init (graph_with_keys, vertex2)
        (*Adds Vertex2 to the existing interferences*)
        val graph_after_v1 =
          Binarymap.insert (graph_with_keys, vertex1, Binaryset.add (inter_vertex1, vertex2))
      in
        (*graph after v2*)
        Binarymap.insert (graph_after_v1, vertex2, Binaryset.add (inter_vertex2, vertex1))
      end


  (*computes interference for vars in single instruction*)
  fun add_edges_for_instr 
    (info : InstrInfo, graph : IGraph) 
    : IGraph =
    let
      val defs = #defs (#ud info)
      val outs = #live_out (#lv info)
    in
      Binaryset.foldl
        (fn (def_var, graph_accum1) =>
          Binaryset.foldl
            (fn (out_var, graph_accum2) => add_undirected_edge (graph_accum2, def_var, out_var))
            graph_accum1
            outs)
        graph
        defs
    end

    (*from liveness: fun analyze*)
  fun build_instr_mapping 
    (instr_map : (TID, InstrInfo) Binarymap.dict) 
    : IGraph =
    Binarymap.foldl
      (fn (_, info, graph_accum) => add_edges_for_instr (info, graph_accum))
      (empty_graph ())
      instr_map

  fun coalesce_boundaries (edge_args : (TID, L4cfg.EdgeArgs) Binarymap.dict) : Preference list =
    (*
     * For each CFG edge, create coalescing preferences from positional
     * source-exit/target-entry argument pairs.
     *)
    Binarymap.foldl
      (fn (_, edge_arg, acc) =>
        ListPair.zipEq (#source_exits edge_arg, #target_entries edge_arg) @ acc)
      []
      edge_args
  (*TODO: *)
  val coalesce_two_addr_instrs = []
    
end
