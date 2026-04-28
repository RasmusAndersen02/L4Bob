signature L4cfg =
sig
  type ID = L4utils.ID
  type TID = L4utils.TID
  type Label = L4utils.Label
  type Var = L4utils.Var

  datatype Dir = Entries | Exits

  type BlockMap = L4utils.BlockMap
  type CFG = L4utils.CFG
  type EdgeArgs = L4utils.EdgeArgs
  type EdgeArgsMap = L4utils.EdgeArgsMap

  (* val label_to_id_mapping : BlockMap * Dir -> (Label, ID) Binarymap.dict *)
  val ids_from_labels : Label list * (Label, ID) Binarymap.dict -> ID list
  val single_node :
    L4.block * (Label, ID) Binarymap.dict * (Label, ID) Binarymap.dict ->
    ID list * ID list

  val build_cfg : L4.block list -> CFG
  val build_edge_args : BlockMap * CFG -> EdgeArgsMap
end
