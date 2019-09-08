open I3ipc.Reply

type t = node

let is_workspace w = function
  | {nodetype = Workspace; name = Some w'; _} when w = w' -> true
  | _ -> false

let get_children {nodes; _} = nodes
