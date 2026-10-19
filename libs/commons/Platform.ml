(* TODO: other options for Windows! *)
type kernel = Darwin | Linux | OtherKernel of string

let kernel (caps : < Cap.exec >) =
  let output =
    match CapExec.string_of_run caps#exec ~trim:true (Cmd.Name "uname", []) with
    | Ok (output, _status) -> output
    | Error (`Msg _) -> ""
  in
  let s = String.lowercase_ascii output in
  match s with
  | "darwin" -> Darwin
  | "linux" -> Linux
  | _ -> OtherKernel s

(* TODO? || Sys.cygwin? *)
let is_windows = Sys.win32
