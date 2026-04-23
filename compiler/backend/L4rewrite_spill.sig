signature L4rewrite_spill =
sig
  type Var = L4utils.Var

  (* Rewrites blocks by spilling variables in [spills]. *)
  val rewrite_blocks : L4.block list * Var list -> L4.block list
end
