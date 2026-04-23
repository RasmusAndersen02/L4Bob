structure L4emit :> L4emit =
struct
  type Var = L4utils.Var
  type Reg = L4regalloc_new.Reg
  type Allocation = L4regalloc_new.Allocation

  fun emit_function (_ : L4.func, _ : Allocation) : string list =
    raise Fail "TODO: emit function"

  fun emit_program (_ : L4.program, _ : (string * Allocation) list) : string =
    raise Fail "TODO: emit program"
end
