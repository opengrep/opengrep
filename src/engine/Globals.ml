(* Yoann Padioleau
 *
 * Copyright (C) 2023 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The goal of this module is mostly to document the "dangerous" globals
 * used inside Semgrep.
 *
 * Ultimately, we want to eliminate all globals. Until now, those globals did
 * not create too many issues because of the use of Parmap and fork
 * in Core_scan.ml (the modifications of those globals in the child process
 * do not affect the state in the parent process), but as soon as we migrate to
 * using domains instead with OCaml 5.0, those globals will haunt us back.
 * Maybe Domain-local-storage globals could help, but even better if we
 * can eliminate them.
 *
 * To find candidates for those "dangerous" globals, you can start with:
 *  $ ./bin/opengrep-cli --experimental -e 'val $V: $T ref' -l ocaml src/ libs/
 *  $ ./bin/opengrep-cli --experimental -e 'let $V: $T = ref $X' -l ocaml src/ libs/
 *
 * We also need to look for things like hash tables and [Lazy.t] and [Str], which are
 * not thread-safe: 
 *  $ ./bin/opengrep-cli --experimental -e 'let $H = Hashtbl.create $I' -l ocaml src/ libs/
 *  $ ./bin/opengrep-cli --experimental -e 'Lazy.force $L' -l ocaml src/ libs/
 *  $ ./bin/opengrep-cli --experimental -e 'Str.$F' -l ocaml src/ libs/
 *  NOTE: Some [Str] functions have been improved to work with less issues in Domains. 
 *
 * And to check use of Lwt which may have issues with memprof-limits cancellation: 
 *  $ ./bin/opengrep-cli -e 'Lwt_platform.$F' -l ocaml src/ libs/
 *  $ ./bin/opengrep-cli -e 'Lwt.$F' -l ocaml src/ libs/
 *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

module TLS = Thread_local_storage

(* Useful for defensive programming, especially in tests which may leave
 * bad state behind.
 * Note that it's currently unused, because we should prefer to fix our tests
 * to restore the globals they modified, but as a last resort, you can
 * use this function.
 *)
let reset () =
  Core_profiling.profiling := false; (* NOTE: Seems we don't need to touch this. *)
  (* TODO: Better to just get rid of global state, this is tricky with domains. *)
  TLS.set Rule.last_matched_rule None; (* DONE *)
  TLS.set Match_patterns.last_matched_rule None; (* NEW, DONE *)
  Pro_hooks.reset_pro_hooks ();
  (* TODO: There is probably more places where we need to do that: *)
  Kcas_data.Hashtbl.clear Xpattern_matcher.hmemo;
  Kcas_data.Hashtbl.clear Range.hmemo;
  Kcas_data.Hashtbl.clear Spacegrep.Find_files.hmemo;
  (* TODO?
   * - the internal parser refs in Parsing_plugin.ml [TODO]
   * - Http_helpers.client_ref ? [TODO]
   * - Std_msg.highlight_xxx [Does not exist]
   * - Logs library state [DONE]
   * - Xpattern.count [DONE]
   * - GenSym.MkId for AST_generic.SId and AST_generic.IdInfoId [DONE]
   * - Tracing.ml active_endpoint [Ignore for now, seems OK since tracing is disabled]
   * - Parmap_targets.ml parmap_child_top_level_span [Does not exist]
   * - Session.ml scan_config_parser_ref [Reference which is not set anywhere, in OSS at least]
   * - many more [So true...]
   *)
  ()
