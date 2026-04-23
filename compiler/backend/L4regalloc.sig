signature L4regalloc =
sig
  type Var = L4utils.Var
  type Reg = int
  type IGraph = L4interference.IGraph
  type Preference = L4interference.Preference

  type Allocation = (Var, Reg) Binarymap.dict

  val empty_allocation : unit -> Allocation

  (* [num_regs] is number of available physical registers. *)
  (* Returns allocation and list of spilled variables. *)
  val color : IGraph * Preference list * int -> Allocation * Var list
end
