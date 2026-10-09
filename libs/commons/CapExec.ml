(* See also CapSys and CapUnix in TCB/.

   See also Cmd.ml which contains the "pure" stuff.
*)

let string_of_run _caps ~trim cmd = UCmd.string_of_run ~trim cmd

let string_of_run_with_stderr _caps ~trim cmd =
  UCmd.string_of_run_with_stderr ~trim cmd

let lines_of_run _caps ~trim cmd = UCmd.lines_of_run ~trim cmd
let status_of_run _caps ?quiet cmd = UCmd.status_of_run ?quiet cmd
