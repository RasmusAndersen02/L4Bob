signature L4utils =
sig
  (* aliases *)
  type ID = int
  type TID = int * int
  type Var = string
  type Label = string

  (* generic structures *)
  type VarSet = Var Binaryset.set

  (* datatypes *)
  datatype Dir = Entries | Exits

  (* records *)
  type UseDef =
    { uses : VarSet
    , defs : VarSet
    }

  type Live =
    { live_in : VarSet
    , live_out : VarSet
    }

  type Boundary =
    { entry_vars_ordered : Var list
    , exit_vars_ordered : Var list
    , entry_vars : VarSet
    , exit_vars : VarSet
    }

  type InstrInfo =
    { id : TID
    , instr : L4.instr
    , ud : UseDef
    , lv : Live
    }

  type EdgeArgs = (L4.decl * L4.decl) list
    (* { source_exit_args : L4.args *)
    (* , target_entry_args : L4.args *)
    (* } *)

  (* maps *)
  type BlockMap = L4.block Intmap.intmap
  type EdgeArgsMap = (TID, EdgeArgs) Binarymap.dict
  type CFG = (ID list * ID list) Intmap.intmap


  (* orders *)
  val tuple_id_ord : TID * TID -> order

  (* initializers *)
  val ini_varset : unit -> VarSet
  val ini_tid_map : unit -> (TID, 'a) Binarymap.dict
  val ini_str_map : unit -> (string, 'a) Binarymap.dict

  (* converters *)
  val from_varlist_to_varset : Var list -> VarSet
  val from_decl_to_var : L4.decl -> Var option
  val from_decllist_to_varlist : L4.decl list -> Var list
  val from_decllist_to_varset : L4.decl list -> VarSet

  (* getters *)
  val get_block_labels : L4.block * Dir -> Label list
  val get_block_decls : L4.block * Dir -> L4.decl list
  val get_block_vars : L4.block * Dir -> VarSet

  (* mappers *)
  val map_id_to_block : L4.block list -> BlockMap
  val map_label_to_id : BlockMap * Dir -> (Label, ID) Binarymap.dict
end
