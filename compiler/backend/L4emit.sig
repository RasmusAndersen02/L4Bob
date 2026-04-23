signature L4emit =
sig
  type Var = L4utils.Var
  type Reg = L4regalloc_new.Reg
  type Allocation = L4regalloc_new.Allocation

  (* Emits one function to a target textual form. *)
  val emit_function : L4.func * Allocation -> string list

  (* Emits an entire program. *)
  val emit_program : L4.program * (string * Allocation) list -> string
end
