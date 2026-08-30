(* TODO: other options for Windows! *)
type kernel = Darwin | Linux | OtherKernel of string

(* TODO: should use CapExec.string_of_run instead of with_open_process_in *)

let kernel (caps : < Cap.exec >) =
  CapExec.with_open_process_in caps#exec "uname" (fun chan ->
      let s =
        In_channel.input_all chan |> String.trim |> String.lowercase_ascii
      in
      match s with
      | "darwin" -> Darwin
      | "linux" -> Linux
      | _ -> OtherKernel s)

(* TODO? || Sys.cygwin? *)
let is_windows = Sys.win32
