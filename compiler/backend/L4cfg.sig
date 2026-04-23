signature L4cfg =
sig
  type ID = L4utils.ID
  type TID = L4utils.TID
  type Label = L4utils.Label
  type Var = L4utils.Var

  datatype Dir = Entries | Exits

  type Cfg = L4utils.Cfg
  type EdgeArgs = L4utils.EdgeArgs
  type EdgeArgsMap = L4utils.EdgeArgsMap

  val decl_to_var : L4.decl -> Var option
  val get_block_labels : L4.block * Dir -> Label list
  val get_block_vars : L4.block * Dir -> Var list
  val get_block_decls : L4.block * Dir -> L4.decl list

  val block_id_mapping : L4.block list -> L4.block Intmap.intmap
  val label_to_id_mapping : L4.block Intmap.intmap * Dir -> (Label, ID) Binarymap.dict
  val ids_from_labels : Label list * (Label, ID) Binarymap.dict -> ID list
  val single_node :
    L4.block * (Label, ID) Binarymap.dict * (Label, ID) Binarymap.dict ->
    ID list * ID list

  val build_cfg : L4.block list -> Cfg
  val build_edge_args : L4.block Intmap.intmap * Cfg -> EdgeArgsMap
end
