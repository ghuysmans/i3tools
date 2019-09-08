open I3ipc.Reply

module M = struct
  type t = node
  let get_children = I3tools.Tree.get_children

  open Notty
  open I

  let image_of_layout = function
    | SplitH -> (* "\xe2\x86\x94" *) string A.(fg green) "Horizontal"
    | SplitV -> (* "\xe2\x86\x95" *) string A.(fg green) "Vertical"
    | Stacked -> string A.(fg blue) "Stacked"
    | Tabbed -> string A.(fg blue) "Tabbed"
    | Dockarea -> string A.(fg red) "Dock area"
    | Output -> string A.(fg red) "Output"
    | Unknown u -> string A.empty u

  let to_image n =
    let ghost = A.(fg (gray 5)) in
    match n with
    | {nodes = []; name = None; _} ->
      string ghost "no name"
    | {nodes = []; name = Some name; _} ->
      string A.empty name
    | {nodetype; layout; name; _} ->
      match nodetype, name with
       | Root, _ ->
         string ghost "Root"
       | Workspace, Some name ->
         string A.(st bold) name <|> string A.empty ". " <|>
         image_of_layout layout
       | _, _ ->
         image_of_layout layout
end

module N = Texttree_notty.Printer.Make (M)
let image_of_layout = M.image_of_layout
let to_image = N.to_image
