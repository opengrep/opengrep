(* Run jobs in parallel on a pre-ordered list of work items; the caller
   orders the items and supplies an exception handler. *)
val map_work_items :
  < Cap.fork > ->
  int (* ncores *) ->
  exception_handler:('a -> Exception.t -> 'err) ->
  ('a -> 'b) ->
  'a list ->
  ('b, 'err) result list

(* Run jobs in parallel, using number of cores specified with -j. *)
val map_targets :
  < Cap.fork > ->
  int (* ncores *) ->
  (Target.t -> 'a) ->
  (* job function *) Target.t list ->
  ('a, Target.t * Core_error.t) result list
