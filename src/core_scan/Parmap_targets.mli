(* Run jobs in parallel, using number of cores specified with -j. *)
val map_targets__run_in_forked_process_do_not_modify_globals :
  < Cap.fork > ->
  int (* ncores *) ->
  (Target.t -> 'a) ->
  (* job function *) Target.t list ->
  ('a, Target.t * Core_error.t) result list
