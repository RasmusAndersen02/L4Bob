structure L4cfg :> L4cfg =
struct
  type BlockId = L4analysis_types.BlockId
  type Label = L4analysis_types.Label
  type Var = L4analysis_types.Var

  type Cfg = (BlockId list * BlockId list) Intmap.intmap

  type EdgeArgs =
    { source : BlockId
    , target : BlockId
    , source_exits : Var list
    , target_entries : Var list
    }

  datatype Dir = Entries | Exits

  fun decl_to_var (L4.VarD (name, _)) : Var option = SOME name
    | decl_to_var (L4.ConstD _) = NONE

  (*Gives each block in AST an integer ID*)
  fun block_id_mapping 
    (blocks : L4.block list) 
    : L4.block Intmap.intmap =
    let
      (*IDs are just ascending from 0 in the order they are plucked from the block list*)
      fun add_block 
        (block : L4.block, (next_id, id_map) : BlockId * L4.block Intmap.intmap)
        : BlockId * L4.block Intmap.intmap =
        (next_id + 1, Intmap.insert (id_map, next_id, block))

      val block_map_accum = (0, Intmap.empty())
      val (_, block_map) = List.foldl 
        add_block 
        block_map_accum 
        blocks
    in
      block_map
    end
  (*destructures the AST block type into its entries and exits*)
  fun get_block_labels (block : L4.block, Entries : Dir) : Label list =
      let val ((entry_labels, _, _), _, _) = block in entry_labels end
    | get_block_labels (block : L4.block, Exits : Dir) : Label list =
      let val (_, _, (_, exit_labels, _)) = block in exit_labels end
  (*Maps every label to their corresponding block. Seperate map for entry and exit*)
  fun label_to_id_mapping 
    (block_map : L4.block Intmap.intmap, dir : Dir)
    : (Label, BlockId) Binarymap.dict =
    let
      (*inserts mapping for a single block due to potential multi-entry.
      * label-map contains previous blocks as well*)
      fun add_block_labels
        (id : BlockId, block : L4.block, label_map : (Label, BlockId) Binarymap.dict)
        : (Label, BlockId) Binarymap.dict =
        List.foldl 
          (fn (lbl, acc) => Binarymap.insert (acc, lbl, id))
          label_map
          (get_block_labels (block, dir))
    in
      (*Builds the mapping over all blocks*)
      Intmap.foldl 
        add_block_labels 
        (Binarymap.mkDict String.compare) 
        block_map
    end

  (* Codex 5.3: skip unmatched Begin/End labels instead of raising NotFound. *)
  fun ids_from_labels 
    (lbls : Label list, lbl_map : (Label, BlockId) Binarymap.dict) 
    : BlockId list =
    List.foldl
      (fn (lbl, acc) =>
        case Binarymap.peek (lbl_map, lbl) of
          SOME id => id :: acc
        | NONE => acc)
      []
      lbls
  (*build node in cfg, with the label->block ID mapping lookups for ID -> ID mapping*)
  fun single_node
    (block : L4.block, entry_map : (Label, BlockId) Binarymap.dict, exit_map : (Label, BlockId) Binarymap.dict)
    : BlockId list * BlockId list =
    let
      val entry_labels = get_block_labels (block, Entries)
      val exit_labels = get_block_labels (block, Exits)
      val out_edges = ids_from_labels (exit_labels, entry_map)
      val in_edges = ids_from_labels (entry_labels, exit_map)
    in
      (in_edges, out_edges)
    end

  (*Folds over every block building the CFG from L4 AST*)
  fun build_cfg (blocks : L4.block list) : Cfg =
    let
      val block_map = block_id_mapping blocks
      val entry_map = label_to_id_mapping (block_map, Entries)
      val exit_map = label_to_id_mapping (block_map, Exits)
      (*utilizes single_node but maps it to corresponding block ID*)
      fun add_cfg_node
        (id : BlockId, block : L4.block, cfg_acc : Cfg)
        : Cfg =
        Intmap.insert (cfg_acc, id, single_node (block, entry_map, exit_map))
    in
      Intmap.foldl add_cfg_node (Intmap.empty ()) block_map
    end
  (*like get labels, fetches block inargs and outargs*)
  fun block_exit_vars (block : L4.block) : Var list =
    let val (_, _, (exit_args, _, _)) = block in List.mapPartial decl_to_var exit_args end

  fun block_entry_vars (block : L4.block) : Var list =
    let val ((_, entry_args, _), _, _) = block in List.mapPartial decl_to_var entry_args end

    (*Iterates over cfg with accumulator: List. checks key (block ID) and
    * fetches out args (actuals), maps said out vars to in vars at connected
    * blocks*)
  fun build_edge_args 
    (block_map : L4.block Intmap.intmap, cfg_map : Cfg) 
    : EdgeArgs list =
    let
      fun add_edges_for_block 
        (source_id : BlockId, (_, succs) : BlockId list * BlockId list, accum : EdgeArgs list)
        : EdgeArgs list =
        case Intmap.peek (block_map, source_id) of
          NONE => accum
        | SOME source_block =>
            let
              val source_exits = block_exit_vars source_block
              fun add_edge 
                (target_id : BlockId, edge_accum : EdgeArgs list) 
                : EdgeArgs list =
                case Intmap.peek (block_map, target_id) of
                  NONE => edge_accum
                | SOME target_block =>
                    { source = source_id
                    , target = target_id
                    , source_exits = source_exits
                    , target_entries = block_entry_vars target_block
                    } :: edge_accum
            in
              List.foldl add_edge accum succs
            end
    in
      Intmap.foldl add_edges_for_block [] cfg_map
    end
end
