(*
   Wrapper around Printexc to ensure or at least encourage uniform
   exception tracing.
*)

type t = exn * Printexc.raw_backtrace

let catch exn = (exn, Printexc.get_raw_backtrace ())
let trace exn = (exn, Printexc.get_callstack 100)

let reraise ((exn, trace) : t) = Printexc.raise_with_backtrace exn trace
let catch_and_reraise exn = reraise (catch exn)
let create exn trace = (exn, trace)
let get_exn (exn, _trace) = exn
let get_trace (_exn, trace) = trace

let to_string (exn, trace) =
  let msg =
    Printf.sprintf "%s\n%s" (Printexc.to_string exn)
      (Printexc.raw_backtrace_to_string trace)
  in
  (* ensure the output ends with a newline *)
  if msg = "" || msg.[String.length msg - 1] <> '\n' then msg ^ "\n" else msg

type timeout_info = { name : string; max_duration : float }

exception Timeout of timeout_info
