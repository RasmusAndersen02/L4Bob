signature L4liveness =
sig
  type BlockId = L4analysis_types.BlockId
  type InstrId = L4analysis_types.InstrId
  type Var = L4analysis_types.Var
  type VarSet = L4analysis_types.VarSet
  type Boundary = L4analysis_types.Boundary
  type UseDef = L4analysis_types.UseDef
  type Live = L4analysis_types.Live
  type InstrInfo = L4analysis_types.InstrInfo

  type Cfg = L4cfg.Cfg
  type EdgeArgs = L4cfg.EdgeArgs

  type Result =
    { boundary_by_block : Boundary Intmap.intmap
    , block_live : Live Intmap.intmap
    , instr_live : (InstrId, InstrInfo) Binarymap.dict
    }

  val use_def_of_instr : L4.instr -> UseDef
  val live_of_instr : UseDef * Live -> Live
  val build_boundary : L4.block -> Boundary
  val analyze : L4.block list * Cfg * EdgeArgs list -> Result
end
