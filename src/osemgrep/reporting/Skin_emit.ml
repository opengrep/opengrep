(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Writes the chunks that a skin returns, in its order. The report header
 * and footer go through Logs, so that --quiet and --verbose control what is
 * shown, the level keeps its prefix, and while the status line is drawn
 * they are written between two redraws.
 *)

(* False while logging is off (--quiet): Logs then drops every message, so a
   chunk need not be built. *)
let stderr_is_shown () : bool = Option.is_some (Logs.level ())

let emit ?(on_findings : unit -> unit = fun () -> ())
    (chunks : Skin.chunk list) : unit =
  chunks
  |> List.iter (fun (chunk : Skin.chunk) ->
         match chunk with
         | Skin.Findings -> on_findings ()
         | Skin.Line (level, doc) ->
             Logs.msg level (fun m -> m "%a" (fun ppf () -> doc ppf) ()))
