structure L4utils :> L4utils =
struct
  (*aliases*)
  type ID = int
  type TID = int * int
  type Var = string
  type Label = string
  
  (*generic structures*)
  type VarSet = Var Binaryset.set

  (*datatypes*)
  datatype Dir = Entries | Exits

  (*records*)
  type UseDef =
    { uses : VarSet
    , defs : VarSet }
  type Live =
    { live_in : VarSet
    , live_out : VarSet }
  type Boundary =
    { entry_vars_ordered : Var list
    , exit_vars_ordered : Var list
    , entry_vars : VarSet
    , exit_vars : VarSet }
  type InstrInfo =
    { id : TID
    , instr : L4.instr
    , ud : UseDef
    , lv : Live }
  type EdgeArgs = (L4.decl * L4.decl) list 
    (* { source_exit_args : L4.args *)
    (* , target_entry_args : L4.args *)
    (* } *)
  (*maps*)
  type BlockMap = L4.block Intmap.intmap
  type EdgeArgsMap = (TID, EdgeArgs) Binarymap.dict
  type CFG = (ID list * ID list) Intmap.intmap

  (*orders*)
  fun tuple_id_ord ((b1, i1) : TID, (b2, i2) : TID) : order =
    if b1 < b2 then LESS
    else if b1 > b2 then GREATER
    else if i1 < i2 then LESS
    else if i1 > i2 then GREATER
    else EQUAL

  (*initializers*)
  fun ini_varset () : VarSet = Binaryset.empty String.compare
  fun ini_tid_map () = Binarymap.mkDict tuple_id_ord
  fun ini_str_map () = Binarymap.mkDict String.compare


  (*converters*)
  fun from_varlist_to_varset (vars : Var list) : VarSet =
    Binaryset.addList (ini_varset (), vars)
  fun from_decl_to_var (L4.VarD (name, _)) : Var option = SOME name
    | from_decl_to_var (L4.ConstD _) = NONE
  fun from_decllist_to_varlist (decls : L4.decl list) : Var list = List.mapPartial from_decl_to_var decls
  fun from_decllist_to_varset (decls : L4.decl list) : VarSet =
    from_varlist_to_varset (from_decllist_to_varlist decls)

  (*getters*)

  fun get_block_labels (block : L4.block, Entries : Dir) : Label list =
      let val ((entry_labels, _, _), _, _) = block in entry_labels end
    | get_block_labels (block : L4.block, Exits : Dir) : Label list =
      let val (_, _, (_, exit_labels, _)) = block in exit_labels end
  fun get_block_decls (block : L4.block, Exits: Dir) : L4.decl list =
    let val (_, _, (exit_args, _, _)) = block in exit_args end
    | get_block_decls (block : L4.block, Entries: Dir) : L4.decl list =
    let val ((_, entry_args, _), _, _) = block in entry_args end
  fun get_block_vars (block : L4.block, Exits: Dir) : VarSet =
    let val (_, _, (exit_args, _, _)) = block in from_decllist_to_varset exit_args end
    | get_block_vars (block : L4.block, Entries: Dir) : VarSet =
    let val ((_, entry_args, _), _, _) = block in from_decllist_to_varset entry_args end
  

  (*mappers*)
  fun map_id_to_block
    (blocks : L4.block list) 
    : BlockMap =
    let
      (*IDs are just ascending from 0 in the order they are plucked from the block list*)
      fun add_block 
        (block : L4.block, (next_id, id_map) : ID * BlockMap)
        : ID * BlockMap =
        (next_id + 1, Intmap.insert (id_map, next_id, block))

      val block_map_accum = (0, Intmap.empty())
      val (_, block_map) = List.foldl 
        add_block 
        block_map_accum 
        blocks
    in
      block_map
    end

  fun map_label_to_id
    (block_map : BlockMap, dir : Dir)
    : (Label, ID) Binarymap.dict =
    let
      (*inserts mapping for a single block due to potential multi-entry.
      * label-map contains previous blocks as well*)
      fun add_block_labels
        (id : ID, block : L4.block, label_map : (Label, ID) Binarymap.dict)
        : (Label, ID) Binarymap.dict =
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









end
