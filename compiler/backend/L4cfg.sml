structure L4cfg :> L4cfg =
struct
  type ID = L4utils.ID
  type TID = L4utils.TID
  type Label = L4utils.Label
  type Var = L4utils.Var

  datatype Dir = datatype L4utils.Dir

  type Cfg = L4utils.Cfg
  type EdgeArgs = L4utils.EdgeArgs
  type EdgeArgsMap = (TID, EdgeArgs) Binarymap.dict

  val decl_to_var = L4utils.from_decl_to_var
  val get_block_labels = L4utils.get_block_labels
  val get_block_vars = L4utils.get_block_vars
  val get_block_decls = L4utils.get_block_decls

  val block_id_mapping = L4utils.map_id_to_block
  val label_to_id_mapping = L4utils.map_label_to_id

  (* Skip unmatched labels instead of failing. *)
  fun ids_from_labels
    (lbls : Label list, lbl_map : (Label, ID) Binarymap.dict)
    : ID list =
    List.foldl
      (fn (lbl, accum) =>
        case Binarymap.peek (lbl_map, lbl) of
          SOME id => id :: accum
        | NONE => accum)
      []
      lbls

  fun single_node
    (block : L4.block, entry_map : (Label, ID) Binarymap.dict, exit_map : (Label, ID) Binarymap.dict)
    : ID list * ID list =
    let
      val entry_labels = get_block_labels (block, Entries)
      val exit_labels = get_block_labels (block, Exits)
      val out_edges = ids_from_labels (exit_labels, entry_map)
      val in_edges = ids_from_labels (entry_labels, exit_map)
    in
      (in_edges, out_edges)
    end

  fun build_cfg (blocks : L4.block list) : Cfg =
    let
      val block_map = block_id_mapping blocks
      val entry_map = label_to_id_mapping (block_map, Entries)
      val exit_map = label_to_id_mapping (block_map, Exits)

      fun add_cfg_node (id : ID, block : L4.block, cfg_acc : Cfg) : Cfg =
        Intmap.insert (cfg_acc, id, single_node (block, entry_map, exit_map))
    in
      Intmap.foldl add_cfg_node (Intmap.empty ()) block_map
    end

  fun build_edge_args
    (block_map : L4.block Intmap.intmap, cfg_map : Cfg)
    : EdgeArgsMap =
    let
      val init_edge_map = L4utils.ini_tid_map ()
      (*for every block*)
      fun build_b2b_edge 
        (src_exits: L4.args , target_id: ID)
        : EdgeArgs = 
        let 
          val trg_block = Intmap.retrieve (block_map, target_id)
          val trg_exits = get_block_decls (trg_block, Entries)
        in 
          (* {source_exit_args = src_exits, target_entry_args = trg_exits} *)
          ListPair.zipEq(src_exits, trg_exits)
        end

      fun build_block_edges
        (source_id: ID, (_, succs): ID list * ID list, edge_map: EdgeArgsMap)
        : EdgeArgsMap=
        let
          val src_block = Intmap.retrieve (block_map, source_id)
          val src_exits = get_block_decls (src_block, Exits)
          (* val (_, succs) = Intmap.retrieve (cfg_map, source_id) *)
        in
          List.foldl
            (fn (succ_id, edge_map_i) => 
              let 
                val b2b_edge = build_b2b_edge (src_exits, succ_id) 
              in 
                Binarymap.insert (edge_map_i, (source_id, succ_id), b2b_edge) 
              end)
            edge_map
            succs
        end 
    in
      Intmap.foldl
        (fn (source_id, neighbors, edge_map_i) => 
          build_block_edges (source_id, neighbors, edge_map_i))
        init_edge_map
        cfg_map
    end
end
