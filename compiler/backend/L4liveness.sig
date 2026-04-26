signature L4liveness =
sig
  type ID = L4utils.ID
  type TID = L4utils.TID
  type Var = L4utils.Var
  type VarSet = L4utils.VarSet
  type Boundary = L4utils.Boundary
  type UseDef = L4utils.UseDef
  type Live = L4utils.Live
  type InstrInfo = L4utils.InstrInfo

  type Cfg = L4cfg.Cfg
  type EdgeArgs = L4cfg.EdgeArgs
  type EdgeArgsMap = (TID, EdgeArgs) Binarymap.dict

  type Result =
    { boundary_by_block : Boundary Intmap.intmap
    , block_live : Live Intmap.intmap
    , instr_live : (TID, InstrInfo) Binarymap.dict
    }

  val use_def_of_instr : L4.instr -> UseDef
  val build_instr_live : UseDef * Live -> Live
  val build_boundary : L4.block -> Boundary
  val analyze : L4.block list * Cfg * EdgeArgsMap -> Result
end
