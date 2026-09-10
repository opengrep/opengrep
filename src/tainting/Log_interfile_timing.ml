(* Log source of the interfile phase timings ([Interfile_dispatch.timed],
   [Project_index.timed]); select it alone with
   OPENGREP_LOG_SRCS=semgrep.interfile_timing --verbose to get the phase
   table without the engine's own logging. *)
let src = Logs.Src.create "semgrep.interfile_timing"

module Log = (val Logs.src_log src : Logs.LOG)
