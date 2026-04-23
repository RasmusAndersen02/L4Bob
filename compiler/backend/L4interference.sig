signature L4interference =
sig
  type Var = L4analysis_types.Var
  type VarSet = L4analysis_types.VarSet
  type InstrInfo = L4analysis_types.InstrInfo
  type InstrId = L4analysis_types.InstrId

  type IGraph = (Var, VarSet) Binarymap.dict
  type Preference = Var * Var

  val empty_graph : unit -> IGraph
  val add_undirected_edge : IGraph * Var * Var -> IGraph
  val build_instr_mapping : (InstrId, InstrInfo) Binarymap.dict -> IGraph
  val coalesce_boundaries : L4cfg.EdgeArgs list -> Preference list
end
