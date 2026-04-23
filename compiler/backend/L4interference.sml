structure L4interference :> L4interference =
struct
  type Var = L4analysis_types.Var
  type VarSet = L4analysis_types.VarSet
  type InstrInfo = L4analysis_types.InstrInfo
  type InstrId = L4analysis_types.InstrId

  type IGraph = (Var, VarSet) Binarymap.dict
  type Preference = Var * Var

  fun empty_graph () : IGraph = Binarymap.mkDict String.compare

  fun peek_value_or_init (graph : IGraph, key : Var) : VarSet =
    case Binarymap.peek (graph, key) of
      SOME interferences => interferences
    | NONE => L4analysis_types.empty_varset ()

  fun peek_key_or_insert (graph : IGraph, key : Var) : IGraph =
    case Binarymap.peek (graph, key) of
      SOME _ => graph
    | NONE => Binarymap.insert (graph, key, L4analysis_types.empty_varset ())

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
    (instr_map : (InstrId, InstrInfo) Binarymap.dict) 
    : IGraph =
    Binarymap.foldl
      (fn (_, info, graph_accum) => add_edges_for_instr (info, graph_accum))
      empty_graph()
      instr_map

  fun coalesce_boundaries (edge_args : L4cfg.EdgeArgs list) : Preference list =
    (* Improvement: produce coalescing preferences from positional boundary mapping. *)
    List.concat
      (List.map
        (fn (edge_arg : L4cfg.EdgeArgs) =>
            ListPair.zipEq (#source_exits edge_arg, #target_entries edge_arg))
        edge_args)
  (*TODO: *)
  fun coalesce_two_addr_instrs = []
    
end
