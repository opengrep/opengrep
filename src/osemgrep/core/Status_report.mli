val pp_status :
  rules:Rule.t list ->
  num_targets:int ->
  tracked_by_git:bool ->
  Lang_job.t list ->
  Format.formatter ->
  unit
