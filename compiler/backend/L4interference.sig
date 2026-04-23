signature L4interference =
sig
  type Var = L4utils.Var
  type VarSet = L4utils.VarSet
  type InstrInfo = L4utils.InstrInfo
  type TID = L4utils.TID

  type IGraph = (Var, VarSet) Binarymap.dict
  type Preference = Var * Var

  val empty_graph : unit -> IGraph
  val add_undirected_edge : IGraph * Var * Var -> IGraph
  val build_instr_mapping : (TID, InstrInfo) Binarymap.dict -> IGraph
  val coalesce_boundaries : (TID, L4cfg.EdgeArgs) Binarymap.dict -> Preference list
end
