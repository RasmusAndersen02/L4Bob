structure L4analysis_types :> L4analysis_types =
struct
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

  fun empty_varset () : VarSet = Binaryset.empty String.compare
  fun varset_of_list (vars : Var list) : VarSet =
    Binaryset.addList (empty_varset (), vars)

  fun instr_id_ord ((b1, i1) : InstrId, (b2, i2) : InstrId) : order =
    if b1 < b2 then LESS
    else if b1 > b2 then GREATER
    else if i1 < i2 then LESS
    else if i1 > i2 then GREATER
    else EQUAL
end
