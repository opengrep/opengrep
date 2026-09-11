(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Draws what a skin asked for.
 *
 * The skin decided the order; this only routes each piece to its stream.
 * Stderr goes through Logs, so that --quiet and --verbose keep deciding
 * what is shown and the level keeps its prefix. Stdout is written to
 * Format.std_formatter rather than through CapConsole, so that the colours
 * of the formatter's style renderer survive (see Output.ml).
 *)

(* [on_findings] runs where the skin placed Skin.Findings, i.e. where the
 * report's findings and the diagnostics that go with them belong. *)
let emit ?(on_findings : unit -> unit = fun () -> ())
    (chunks : Skin.chunk list) : unit =
  chunks
  |> List.iter (fun (chunk : Skin.chunk) ->
         match chunk with
         | Skin.Findings -> on_findings ()
         | Skin.Line (Skin.Stdout, doc) ->
             (* nosemgrep: forbid-console *)
             doc Format.std_formatter
         | Skin.Line (Skin.Stderr level, doc) ->
             Logs.msg level (fun m -> m "%a" (fun ppf () -> doc ppf) ()))
