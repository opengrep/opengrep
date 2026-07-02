(* Thin CLI: eval the library's cmdliner [cmd] (no top-level side effects). *)

let () = exit (Cmdliner.Cmd.eval Opengrep_project_index.Main.cmd)
