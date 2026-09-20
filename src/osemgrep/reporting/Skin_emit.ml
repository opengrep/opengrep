(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Draws what a skin asked for.
 *
 * The skin decided the order; this only writes each piece. The chrome goes
 * through Logs, so that --quiet and --verbose keep deciding what is shown
 * and the level keeps its prefix, and so that while the status bar is up
 * it is written between two of its frames.
 *)

(* Whether a chunk can be shown at all. Logs drops every message
   while logging is off, as --quiet turns it, so what only such a chunk
   would show need not be built. *)
let stderr_is_shown () : bool = Option.is_some (Logs.level ())

(* [on_findings] runs where the skin placed Skin.Findings, i.e. where the
 * report's findings and the diagnostics that go with them belong. *)
let emit ?(on_findings : unit -> unit = fun () -> ())
    (chunks : Skin.chunk list) : unit =
  chunks
  |> List.iter (fun (chunk : Skin.chunk) ->
         match chunk with
         | Skin.Findings -> on_findings ()
         | Skin.Line (level, doc) ->
             Logs.msg level (fun m -> m "%a" (fun ppf () -> doc ppf) ()))
