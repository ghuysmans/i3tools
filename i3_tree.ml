module U = Texttree.Utils.Make (I3tools.Tree)

let main c w = Lwt_main.run (
  let%lwt conn = I3ipc.connect () in
  let%lwt root = I3ipc.get_tree conn in
  let root' =
    match w with
    | None -> root
    | Some w ->
      match U.find_opt (I3tools.Tree.is_workspace w) root with
      | Some n -> n
      | None ->
        Printf.fprintf stderr "No such workspace: %S.\n" w;
        exit 1
  in
  if c then (
    I3tools_notty.Printer.to_image root' |> Notty_unix.output_image;
    print_newline ();
    Lwt.return_unit
  )
  else
    I3tools.Printer.to_string root' |> Lwt_io.print
)


open Cmdliner

let color =
  let doc = "Use colors" in
  Arg.(value & flag & info ["c"; "colors"] ~doc)

let workspace =
  let doc = "Focus on a specific workspace" in
  Arg.(value & opt (some string) None & info ["w"; "workspace"] ~doc) 

let main_t =
  Term.(const main $ color $ workspace)

let () = Term.(exit @@ eval (main_t, info "i3_tree"))
