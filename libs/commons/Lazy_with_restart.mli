(* A lazy value that does NOT memoize exceptions.

   [Stdlib.Lazy] stores the exception raised while forcing into the thunk, so
   every subsequent [Lazy.force] re-raises that same exception (see the note in
   the manual: "If it raised an exception, the same exception is raised again").
   That is a problem when the forced computation can be interrupted by an
   *asynchronous* exception (e.g. a per-rule timeout interrupt from
   memprof-limits): a transient failure gets baked into the thunk permanently
   and is re-raised by every later reader of the shared value.

   [Lazy_with_restart] only memoizes on *success*. If forcing raises, the thunk
   is left unforced, so the next [force] re-runs ("restarts") the computation.

   Because forcing can restart, the suspended computation may run more than once
   (whenever a previous force raised), so it should be free of non-idempotent
   side effects.

   NOT concurrency-safe. This module is meant to be forced by a single forcer at
   a time (opengrep forces each [Xtarget] from a single task/domain). A
   concurrent [force] will not crash and will not observe a torn value (under the
   OCaml 5 memory model a reader sees either the unforced thunk or a fully
   initialised result). But it also does nothing to prevent two forcers from
   both running the computation and each installing its own result: for a large
   value such as an AST that silently yields two live copies for the same target
   (extra memory charged against [--max-memory], and physically distinct tokens
   handed to different callers), with no error to signal it — precisely the
   situation [Stdlib.Lazy] turns into a loud [Lazy.Undefined]. Do not force the
   same value from multiple threads or domains. *)

type 'a t

val from_fun : (unit -> 'a) -> 'a t
(** [from_fun f] is a suspension of [f]. [f] is not run until the value is forced.
    Replaces the [lazy (expr)] keyword: write [from_fun (fun () -> expr)]. *)

val from_val : 'a -> 'a t
(** [from_val v] is an already-forced suspension of the value [v]. *)

val force : 'a t -> 'a
(** [force t] returns the value of the suspension, computing it on the first
    call. If the computation raises, the exception is propagated and the
    suspension is left unforced, so a later [force] retries it. *)

val is_val : 'a t -> bool
(** [is_val t] is [true] iff [t] has already been forced successfully (i.e. its
    computation returned without raising). *)
