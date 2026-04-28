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
  datatype Dir = datatype L4utils.Dir

  type BlockMap = L4utils.BlockMap
  type CFG = L4utils.CFG
  type EdgeArgs = L4cfg.EdgeArgs
  type EdgeArgsMap = L4cfg.EdgeArgsMap

  type Liveness =
    { boundary_by_block : Boundary Intmap.intmap
    , block_live : Live Intmap.intmap
    , instr_live : (TID, InstrInfo) Binarymap.dict
    }

  fun build_boundary (block : L4.block) : Boundary =
    let
      val ((_, entry_args, _), _, (exit_args, _, _)) = block
      val entry_vars_ordered = L4utils.from_decllist_to_varlist entry_args
      val exit_vars_ordered = L4utils.from_decllist_to_varlist exit_args
    in
      { entry_vars_ordered = entry_vars_ordered
      , exit_vars_ordered = exit_vars_ordered
      , entry_vars = L4utils.from_varlist_to_varset entry_vars_ordered
      , exit_vars = L4utils.from_varlist_to_varset exit_vars_ordered
      }
    end

  fun use_def_of_instr (L4.Prim (_, in_args, ro_args, out_args, _)) : UseDef =
      { uses = L4utils.from_decllist_to_varset (in_args @ ro_args)
      , defs = L4utils.from_decllist_to_varset out_args }
    | use_def_of_instr (L4.Call (_, in_args, ro_args, out_args, _)) : UseDef =
      { uses = L4utils.from_decllist_to_varset (in_args @ ro_args)
      , defs = L4utils.from_decllist_to_varset out_args }
    | use_def_of_instr (L4.Uncall (_, in_args, ro_args, out_args, _)) : UseDef =
      { uses = L4utils.from_decllist_to_varset (in_args @ ro_args)
      , defs = L4utils.from_decllist_to_varset out_args }

  fun build_instr_live (use_def : UseDef, live_succ : Live) : Live =
    let
      val uses = #uses use_def
      val defs = #defs use_def
      val out_set = #live_in live_succ
      val in_set = Binaryset.union (uses, Binaryset.difference (out_set, defs))
    in
      { live_in = in_set, live_out = out_set }
  end

  fun block_in_from_out ((_, instrs, _) : L4.block, init_out_set : VarSet) : VarSet =
    List.foldr
      (fn (instr, out_set_accum) =>
        let
          val use_def = use_def_of_instr instr
        in
          Binaryset.union (#uses use_def, Binaryset.difference (out_set_accum, #defs use_def))
        end)
      init_out_set
      instrs
  (* given a successor live-in, calculates and maps partial live-out (contributions from that succ only) *)


  (* Calculates fixed point with init_seed as the exit args *)
  fun fixed_point_seed
    (block_map: BlockMap, cfg: CFG, edge_args: EdgeArgsMap)
    : Live Intmap.intmap =
    let
      fun edge_live_transfer
        (edge: EdgeArgs, succ_live_in: VarSet)
        : VarSet =
        let 
          val live_edges =
            List.filter
              (fn (_, trg_decl) => 
                case L4utils.from_decl_to_var(trg_decl) of
                  SOME var => Binaryset.member (succ_live_in, var)
                | NONE => false )
              edge
          val (source_live_decls, _) = ListPair.unzip live_edges 
          val source_live_vars = L4utils.from_decllist_to_varset (source_live_decls) 
          val (_, target_vars) = ListPair.unzip edge
          val passthroughs = Binaryset.difference (succ_live_in , L4utils.from_decllist_to_varset target_vars)
        in 
          Binaryset.union (source_live_vars, passthroughs)
        (* The check with .member is likely redundant. If the successor entry argument is not in the successor live in, it implies it will with certainty not be used in the future. However it is still declared on entry and by its abscense in livein: Immediately dead, Therefore can never be consumed breaking the single consumption invariant *)
      end

      fun merge_succs
        (source_id: ID, live_map: Live Intmap.intmap)
        : VarSet =
        let
          val (_,succ_ids) = Intmap.retrieve (cfg, source_id)
          val src_out_set = List.foldl
            (fn (succ_id, out_accum) => 
              let
                val edge = case Binarymap.peek (edge_args, (source_id, succ_id)) of
                  SOME edge => edge
                | NONE => raise Fail "Edge not present in EdgeArgsMap"
                val succ_live_in = #live_in (Intmap.retrieve (live_map, succ_id))
                val transfer = edge_live_transfer (edge, succ_live_in)
              in 
                
                Binaryset.union (out_accum, transfer )
              end)
            (L4utils.ini_varset ())
            succ_ids
        in
          src_out_set
      end
      
      fun gen_new_live
        (prev_live_map: Live Intmap.intmap)
        : Live Intmap.intmap =
        Intmap.foldr
          (fn (source_id, block, live_map_accum) =>
            let
              val curr_live_set = Intmap.retrieve (live_map_accum, source_id)
              val curr_in_set = #live_in curr_live_set
              val curr_out_set = #live_out curr_live_set
              val new_out_set = merge_succs (source_id, live_map_accum) 
              val new_in_set = block_in_from_out (block, new_out_set)
              val combined_live : Live = 
                { live_in = Binaryset.union(curr_in_set, new_in_set) 
                , live_out = Binaryset.union (curr_out_set, new_out_set)}
            in
              Intmap.insert(live_map_accum, source_id, combined_live)
            end)
        prev_live_map
        block_map

      val init_seed = 
        Intmap.map 
          (fn (_, block) =>
            let 
              val live_out = L4utils.get_block_vars (block, Exits)
              val live_in = block_in_from_out (block, live_out)
            in
            { live_in = live_in
            , live_out = live_out }
          end) 
          block_map

      fun compare_live (live_a: Live Intmap.intmap, live_b: Live Intmap.intmap)
        : bool =
        let
          val live_a_list = Intmap.listItems (live_a)
          val live_b_list = Intmap.listItems (live_b)
        in
          ListPair.all
            (fn (a, b) => 
              let 
                val (a_id, a_live_set) = a
                val (b_id, b_live_set) = b
              in
                a_id = b_id 
                andalso Binaryset.equal (#live_in a_live_set, #live_in b_live_set) 
                andalso Binaryset.equal (#live_out a_live_set, #live_out b_live_set) 
            end)
            (live_a_list, live_b_list)
      end

      fun iterate_till_fixed (live_map: Live Intmap.intmap)
        : Live Intmap.intmap =
        let 
          val new_live_map = gen_new_live (live_map)
          val is_fixed = compare_live (live_map, new_live_map)
        in 
          if is_fixed then new_live_map else iterate_till_fixed (new_live_map)
      end
    in
      iterate_till_fixed (init_seed)
  end

  fun analyze
    (blocks : L4.block list, cfg : CFG, edge_args : EdgeArgsMap)
    : Result =
    let
      val block_map = L4utils.map_id_to_block blocks
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
          val init_out = #live_out (Intmap.retrieve (block_live, block_id))
          val init_instr_id = List.length instrs - 1

          val (updated_map, _, _) =
            List.foldr
              (fn (instr, (instr_lv_map, instr_id, out_set)) =>
                let
                  val ud : UseDef = use_def_of_instr instr
                  val lv_seed : Live = { live_in = out_set, live_out = L4utils.ini_varset () }
                  val lv : Live = build_instr_live (ud, lv_seed)
                  val in_set = #live_in lv
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
