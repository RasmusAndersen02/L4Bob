structure L4regalloc :> L4regalloc =
struct
  type Var = L4utils.Var
  type Reg = int
  type IGraph = L4interference.IGraph
  type Preference = L4interference.Preference

  type Allocation = (Var, Reg) Binarymap.dict

  fun empty_allocation () : Allocation = Binarymap.mkDict String.compare

  fun color (_ : IGraph, _ : Preference list, _ : int) : Allocation * Var list =
    raise Fail "TODO: graph coloring register allocator"
end
