(* The legacy rendering of the scan plan: the "Scan Status" heading, the
   counts of files and rules, and the per-language and per-origin tables.
   Build the plan with Scan_plan.of_lang_jobs. *)
val pp_status : Skin_model.Plan.t Fmt.t
