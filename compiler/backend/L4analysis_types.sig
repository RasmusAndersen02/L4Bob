signature L4analysis_types =
sig
  type BlockId = int
  type Var = string
  type Label = string
  type InstrId = BlockId * int

  type VarSet = Var Binaryset.set

  type Boundary =
    { entry_vars_ordered : Var list
    , exit_vars_ordered : Var list
    , entry_vars : VarSet
    , exit_vars : VarSet
    }

  type UseDef =
    { uses : VarSet
    , defs : VarSet
    }

  type Live =
    { live_in : VarSet
    , live_out : VarSet
    }

  type InstrInfo =
    { id : InstrId
    , instr : L4.instr
    , ud : UseDef
    , lv : Live
    }

  val empty_varset : unit -> VarSet
  val varset_of_list : Var list -> VarSet
  val instr_id_ord : InstrId * InstrId -> order
end
