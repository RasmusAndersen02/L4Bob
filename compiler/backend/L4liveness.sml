structure L4liveness :> L4liveness =
struct
  type ID = L4utils.ID
  type TID = L4utils.TID
  type Var = L4utils.Var
  type VarSet = L4utils.VarSet
  type Boundary = L4utils.Boundary
  type UseDef = L4utils.UseDef
  type Live = L4utils.Live
  type InstrInfo = L4utils.InstrInfo

  type Cfg = L4cfg.Cfg
  type EdgeArgs = L4cfg.EdgeArgs
  type EdgeArgsMap = L4cfg.EdgeArgsMap

  type Result =
    { boundary_by_block : Boundary Intmap.intmap
    , block_live : Live Intmap.intmap
    , instr_live : (TID, InstrInfo) Binarymap.dict
    }

  val vars_of_decls = L4utils.from_decllist_to_varlist

  val from_decllist_to_varset = L4utils.from_decllist_to_varset

fun build_boundary (block : L4.block) : Boundary =
    let
      val ((_, entry_args, _), _, (exit_args, _, _)) = block
      val entry_vars_ordered = vars_of_decls entry_args
      val exit_vars_ordered = vars_of_decls exit_args
    in
      { entry_vars_ordered = entry_vars_ordered
      , exit_vars_ordered = exit_vars_ordered
      , entry_vars = L4utils.from_varlist_to_varset entry_vars_ordered
      , exit_vars = L4utils.from_varlist_to_varset exit_vars_ordered
      }
    end

  fun use_def_of_instr (L4.Prim (_, in_args, ro_args, out_args, _)) : UseDef =
      { uses = set_of_decls (in_args @ ro_args)
      , defs = set_of_decls out_args }
    | use_def_of_instr (L4.Call (_, in_args, ro_args, out_args, _)) : UseDef =
      { uses = set_of_decls (in_args @ ro_args)
      , defs = set_of_decls out_args }
    | use_def_of_instr (L4.Uncall (_, in_args, ro_args, out_args, _)) : UseDef =
      { uses = set_of_decls (in_args @ ro_args)
      , defs = set_of_decls out_args }

  fun build_instr_live (use_def : UseDef, live_succ : Live) : Live =
    let
      val uses = #uses use_def
      val defs = #defs use_def
      val out_set = #live_in live_succ
      val in_set = Binaryset.union (uses, Binaryset.difference (out_set, defs))
    in
      { live_in = in_set, live_out = out_set }
    end

  fun empty_set () : VarSet = L4utils.ini_varset ()

  fun set_equal (s1 : VarSet, s2 : VarSet) : bool =
    Binaryset.isEmpty (Binaryset.difference (s1, s2))
    andalso
    Binaryset.isEmpty (Binaryset.difference (s2, s1))

  fun block_in_from_out (block : L4.block, init_out_set : VarSet) : VarSet =
    let
      val (_, instrs, _) = block
    in
      List.foldr
        (fn (instr, out_set_accum) =>
          let
            val ud = use_def_of_instr instr
          in
            Binaryset.union (#uses ud, Binaryset.difference (out_set_accum, #defs ud))
          end)
        init_out_set
        instrs
    end

  (*
   * Maps successor IN requirements from target-side names back to source-side names
   * using positional boundary arguments. Variables not in target entry are treated
   * as passthrough outliers and kept as-is.
   *)
  fun edge_live_transfer
    (edge_args: EdgeArgsMap, source_id: ID, target_id: ID, succ_live_in: VarSet)
    : VarSet =
    let
      fun map_edge_args (edge: EdgeArgs) : VarSet =
        let 
          val (live_part, dead_part) =
            List.partition
              (fn (_, trg_decl) => 
                Binaryset.member (succ_live_in, L4utils.from_decl_to_var
                (trg_decl)))
              edge
          val (source_live_decls, target_live_decls) = live_part 
          val (_, target_dead_decls) = dead_part 
          val source_live_vars = L4utils.from_decllist_to_varset (source_live_decls) 
          val source_dead_vars = L4utils.from_decllist_to_varset (target_dead_decls) 
          val passthroughs = Binaryset.difference (succ_live_in, dead_part)
        in 
          Binaryset.union (source_live_vars, passthroughs)
      end

      (* is there an edge between the blocks  *)
      fun check_edge () = 
        case Binarymap.peek (edge_args, (source_id, target_id)) of
          NONE => L4utils.ini_varset ()
        | SOME edge => map_edge_args (edge) 
      fun 

        

    in

  end

  (*
   * Fixed-point block liveness seed.
   * OUT[B] starts from block exit vars and is expanded by successor needs.
   * IN[B] is computed from OUT[B] by scanning block instructions backwards.
   *)
  fun fixed_point_seed
    (block_map : L4.block Intmap.intmap, cfg : Cfg, edge_args : EdgeArgsMap)
    : Live Intmap.intmap =
    let
      val boundary_by_block : Boundary Intmap.intmap =
        Intmap.map (fn (_, block) => build_boundary block) block_map

      fun init_live (_, bnd : Boundary) : Live =
        { live_in = empty_set (), live_out = #exit_vars bnd }

      val init_live_map : Live Intmap.intmap =
        Intmap.map init_live boundary_by_block

      fun sweep_once (live_map : Live Intmap.intmap) : Live Intmap.intmap * bool =
        Intmap.foldl
          (fn (block_id, (_, succs), (acc_map, changed_any)) =>
            let
              val block = Intmap.retrieve (block_map, block_id)
              val bnd = Intmap.retrieve (boundary_by_block, block_id)
              val old_live = Intmap.retrieve (acc_map, block_id)

              val succ_need =
                List.foldl
                  (fn (succ_id, need_acc) =>
                    let
                      val succ_live = Intmap.retrieve (acc_map, succ_id)
                      val contrib = edge_contrib (edge_args, block_id, succ_id, #live_in succ_live)
                    in
                      Binaryset.union (need_acc, contrib)
                    end)
                  (empty_set ())
                  succs

              val new_out = Binaryset.union (#exit_vars bnd, succ_need)
              val new_in = block_in_from_out (block, new_out)

              val changed_here =
                (not (set_equal (#live_in old_live, new_in)))
                orelse
                (not (set_equal (#live_out old_live, new_out)))

              val new_live : Live = { live_in = new_in, live_out = new_out }
            in
              (Intmap.insert (acc_map, block_id, new_live), changed_any orelse changed_here)
            end)
          (live_map, false)
          cfg

      fun iterate (live_map : Live Intmap.intmap) : Live Intmap.intmap =
        let
          val (next_map, changed) = sweep_once live_map
        in
          if changed then iterate next_map else next_map
        end
    in
      iterate init_live_map
    end

  fun analyze
    (blocks : L4.block list, cfg : Cfg, edge_args : EdgeArgsMap)
    : Result =
    let
      val block_map = L4cfg.block_id_mapping blocks
      val boundary_by_block : Boundary Intmap.intmap =
        Intmap.map (fn (_, block) => build_boundary block) block_map

      (* Block-level liveness solved by fixed point over the CFG. *)
      val block_live : Live Intmap.intmap =
        fixed_point_seed (block_map, cfg, edge_args)

      val init_instr_live : (TID, InstrInfo) Binarymap.dict =
        L4utils.ini_tid_map ()

      fun analyze_block_instrs
        (block_id : ID, block : L4.block, instr_accum : (TID, InstrInfo) Binarymap.dict)
        : (TID, InstrInfo) Binarymap.dict =
        let
          val (_, instrs, _) = block
          (* Seed instruction analysis with solved block OUT, not raw boundary exit. *)
          val init_out = #live_out (Intmap.retrieve (block_live, block_id))
          val init_instr_id = List.length instrs - 1

          val (updated_map, _, _) =
            List.foldr
              (fn (instr, (instr_lv_map, instr_id, out_set)) =>
                let
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

      val instr_live : (TID, InstrInfo) Binarymap.dict =
        Intmap.foldl analyze_block_instrs init_instr_live block_map
    in
      { boundary_by_block = boundary_by_block
      , block_live = block_live
      , instr_live = instr_live
      }
    end
end
