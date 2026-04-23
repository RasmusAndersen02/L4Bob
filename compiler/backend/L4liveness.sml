structure L4liveness :> L4liveness =
struct
  type BlockId = L4analysis_types.BlockId
  type InstrId = L4analysis_types.InstrId
  type Var = L4analysis_types.Var
  type VarSet = L4analysis_types.VarSet
  type Boundary = L4analysis_types.Boundary
  type UseDef = L4analysis_types.UseDef
  type Live = L4analysis_types.Live
  type InstrInfo = L4analysis_types.InstrInfo

  type Cfg = L4cfg.Cfg
  type EdgeArgs = L4cfg.EdgeArgs

  type Result =
    { boundary_by_block : Boundary Intmap.intmap
    , block_live : Live Intmap.intmap
    , instr_live : (InstrId, InstrInfo) Binarymap.dict
    }

  (*Returns option to filter out constants*)
  fun decl_to_var (L4.VarD (name, _)) : Var option = SOME name
    | decl_to_var (L4.ConstD _) = NONE

  (*Hence we use mapPartial to ignore said constants and get name from SOME _*)
  fun vars_of_decls (decls : L4.decl list) : Var list = List.mapPartial decl_to_var decls
  (*useful representation due to native set operations, but otherwise carry same
  * info as its list*)
  fun set_of_decls (decls : L4.decl list) : VarSet =
    L4analysis_types.varset_of_list (vars_of_decls decls)

  fun use_def_of_instr (L4.Prim (_, in_args, ro_args, out_args, _)) : UseDef =
      { uses = set_of_decls (in_args @ ro_args)
      , defs = set_of_decls out_args
      }
    | use_def_of_instr (L4.Call (_, in_args, ro_args, out_args, _)) : UseDef =
      (* Codex 5.3: Improvement: include calls/uncalls in same USE/DEF model as prims. *)
      { uses = set_of_decls (in_args @ ro_args)
      , defs = set_of_decls out_args
      }
    | use_def_of_instr (L4.Uncall (_, in_args, ro_args, out_args, _)) : UseDef =
      { uses = set_of_decls (in_args @ ro_args)
      , defs = set_of_decls out_args
      }
  fun live_of_instr (use_def : UseDef, live_succ : Live) : Live =
    let
      val uses = #uses use_def
      val defs = #defs use_def
      val out_set = #live_in live_succ
      val in_set = Binaryset.union (uses, Binaryset.difference (out_set, defs))
    in
      {live_in = in_set, live_out = out_set}
    end

  fun build_boundary (block : L4.block) : Boundary =
    let
      val ((_, entry_args, _), _, (exit_args, _, _)) = block
      val entry_vars_ordered = vars_of_decls entry_args
      val exit_vars_ordered = vars_of_decls exit_args
    in
      { entry_vars_ordered = entry_vars_ordered
      , exit_vars_ordered = exit_vars_ordered
      , entry_vars = L4analysis_types.varset_of_list entry_vars_ordered
      , exit_vars = L4analysis_types.varset_of_list exit_vars_ordered
      }
    end

    (*
    *
  fun build_block_live
    (block_map: L4.block Intmap.intmap)
    : Live Intmap.intmap =
    let
      val 
    * *)

  fun analyze 
    (blocks : L4.block list, _ : Cfg, _ : EdgeArgs list) 
    : Result =
    let
      val block_map = L4cfg.block_id_mapping blocks
      val boundary_by_block : Boundary Intmap.intmap =
        Intmap.map (fn (_, block) => build_boundary block) block_map
      (*builds type live Intmap*)
      val block_live : Live Intmap.intmap =
        Intmap.map
          (fn (_, bnd) => { live_in = #entry_vars bnd, live_out = #exit_vars bnd })
          boundary_by_block

      val init_instr_live : (InstrId, InstrInfo) Binarymap.dict =
        Binarymap.mkDict L4analysis_types.instr_id_ord

      fun analyze_block_instrs
        (block_id : BlockId, block : L4.block, instr_accum : (InstrId, InstrInfo) Binarymap.dict)
        : (InstrId, InstrInfo) Binarymap.dict =
        let
          val (_, instrs, _) = block
          val boundary = Intmap.retrieve (boundary_by_block, block_id)
          val init_out = #exit_vars boundary
          (*due to foldr which gets the last list em first, hence IDs are in
            * order of appearence*)
          val init_instr_id = List.length instrs - 1

          val (updated_map, _, _) =
            List.foldr
              (fn (instr, (instr_lv_map, instr_id, out_set)) =>
                let
                  (*dataflow equations-ish*)
                  val ud : UseDef = use_def_of_instr instr
                  val uses = #uses ud
                  val defs = #defs ud
                  val in_set = Binaryset.union (uses, Binaryset.difference (out_set, defs))
                  val lv : Live = { live_in = in_set, live_out = out_set }
                  val info : InstrInfo = { id = (block_id, instr_id), instr = instr, ud = ud, lv = lv }
                  val new_instr_lv_map = Binarymap.insert (instr_lv_map, (block_id, instr_id), info)
                in
                  (new_instr_lv_map, instr_id - 1, in_set)
                end)
              (instr_accum, init_instr_id, init_out)
              instrs
        in
          updated_map
        end

      (*does liveness analyses (in/out def/use) sets for every instruction in
        * every block*)
      val instr_live 
        : (InstrId, InstrInfo) Binarymap.dict =
        Intmap.foldl analyze_block_instrs init_instr_live block_map
      (* Improvement: keep block-level and instruction-level maps separate. *)
    in
      { boundary_by_block = boundary_by_block
      , block_live = block_live
      , instr_live = instr_live
      }
    end
end
