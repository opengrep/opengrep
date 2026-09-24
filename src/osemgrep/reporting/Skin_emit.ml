(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Draws what a skin asked for.
 *
 * The skin decided the order; this only routes each piece to its stream.
 * Stderr goes through Logs, so that --quiet and --verbose keep deciding
 * what is shown and the level keeps its prefix. Stdout is written to
 * Format.std_formatter rather than through CapConsole, so that the colours
 * of the formatter's style renderer survive (see Output.ml), and is held
 * apart from the status bar by hand, since it takes none of the Logs path
 * that does so.
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
             (* A chunk bound for stdout still shares a terminal with the
                status bar, which draws on stderr, and unlike a Stderr
                chunk it does not pass through Logs -- the path that erases
                the bar before a message and puts it back after. So it
                holds the same lock and runs the same hooks itself.
                [on_start] and [on_plan] both emit while the bar is up.

                Flushed inside the lock as well: what stays in the
                formatter's buffer surfaces at some later flush, by then
                in the middle of whatever has been drawn since. *)
             Logs_.with_reporter_lock (fun () ->
                 (* nosemgrep: forbid-console *)
                 doc Format.std_formatter;
                 Format.pp_print_flush Format.std_formatter ())
         | Skin.Line (Skin.Stderr level, doc) ->
             Logs.msg level (fun m -> m "%a" (fun ppf () -> doc ppf) ()))
