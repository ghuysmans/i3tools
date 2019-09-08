open I3ipc.Reply

module M = struct
  type t = node
  let get_children = Tree.get_children

  (*
  let string_of_layout l = Format.asprintf "%a" pp_node_layout l
  *)

  let string_of_layout = function
    | SplitH -> "Horizontal"
    | SplitV -> "Vertical"
    | Stacked -> "Stacked"
    | Tabbed -> "Tabbed"
    | Dockarea -> "Dock area"
    | Output -> "Output"
    | Unknown u -> u

  let to_string n =
    match n with
    | {nodes = []; name = None; _} -> "no name"
    | {nodes = []; name = Some name; _} -> name
    | {nodetype; layout; name; _} ->
      match nodetype, name with
       | Root, _ -> "Root"
       | Workspace, Some name -> (name ^ ". ") ^ string_of_layout layout
       | _, _ -> string_of_layout layout
end

module N = Texttree.Printer.Make (M)
let string_of_layout = M.string_of_layout
let to_string = N.to_string
