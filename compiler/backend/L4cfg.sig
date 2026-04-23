signature L4cfg =
sig
  type BlockId = L4analysis_types.BlockId
  type Label = L4analysis_types.Label
  type Var = L4analysis_types.Var

  type Cfg = (BlockId list * BlockId list) Intmap.intmap

  type EdgeArgs =
    { source : BlockId
    , target : BlockId
    , source_exits : Var list
    , target_entries : Var list
    }

  val block_id_mapping : L4.block list -> L4.block Intmap.intmap
  val build_cfg : L4.block list -> Cfg
  val build_edge_args : L4.block Intmap.intmap * Cfg -> EdgeArgs list
end
