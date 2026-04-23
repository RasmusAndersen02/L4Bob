structure L4rewrite_spill :> L4rewrite_spill =
struct
  type Var = L4utils.Var

  fun rewrite_blocks (_ : L4.block list, _ : Var list) : L4.block list =
    raise Fail "TODO: spill rewrite"
end
