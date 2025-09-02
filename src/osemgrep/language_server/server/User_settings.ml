(* Austin Theriault
 *
 * Copyright (C) 2019-2023 Semgrep, Inc.
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

(* Commentary *)
(* User facing settings. Should match all applicable scan settings in *)
(* package.json of the VSCode extension *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Code *)
(*****************************************************************************)
type t = {
  configuration : string list; [@default []]
  exclude : string list; [@default []]
  include_ : string list; [@key "include"] [@default []]
  jobs : int; [@default Domainslib_.get_cpu_count()]
  max_memory : int; [@key "maxMemory"] [@default 0]
  max_match_per_file : int;
    [@key "maxMatchPerFile"] [@default Core_scan_config.default.max_match_per_file]
  max_target_bytes : int; [@key "maxTargetBytes"] [@default 1000000]
  timeout : int; [@default 30]
  allow_rule_timeout_control : bool;
    [@key "allowRuleTimeoutControl"] [@default false]
  dynamic_timeout : bool
    [@key "dynamicTimeout"] [@default false];
  dynamic_timeout_max_multiplier : int
    [@key "dynamicTimeoutMaxMultiplier"]
    [@default Core_scan_config.default.dynamic_timeout_max_multiplier];
  dynamic_timeout_unit_kb : int
    [@key "dynamicTimeoutUnitKb"]
    [@default Core_scan_config.default.dynamic_timeout_unit_kb];
  timeout_threshold : int; [@key "timeoutThreshold"] [@default 3]
  only_git_dirty : bool; [@key "onlyGitDirty"] [@default true]
  ci : bool; [@default true]
  do_hover : bool; [@default false]
  pro_intrafile : bool; [@default false]
}
[@@deriving yojson]

let default = Yojson.Safe.from_string "{}" |> of_yojson |> Result.get_ok
let t_of_yojson json = of_yojson json
let yojson_of_t settings = to_yojson settings
let pp fmt settings = Yojson.Safe.pretty_print fmt (yojson_of_t settings)

let find_targets_conf_of_t settings : Find_targets.conf =
  let include_ =
    if settings.include_ <> [] then Some settings.include_ else None
  in
  {
    Find_targets.default_conf with
    exclude = settings.exclude;
    include_;
    max_target_bytes = settings.max_target_bytes;
    (* TODO: explain or use the default value of default_conf.diff_depth *)
    diff_depth = 0;
    (* If you're editing minified files then ???
       TODO: explain or use the same default as in default_conf. *)
    exclude_minified_files = true;
  }

let core_runner_conf_of_t settings : Core_runner.conf =
  Core_runner.
    {
      num_jobs = settings.jobs;
      optimizations = true;
      max_memory_mb = settings.max_memory;
      max_match_per_file = settings.max_match_per_file;
      timeout = float_of_int settings.timeout;
      dynamic_timeout = settings.dynamic_timeout;
      dynamic_timeout_max_multiplier = settings.dynamic_timeout_max_multiplier;
      dynamic_timeout_unit_kb = settings.dynamic_timeout_unit_kb;
      allow_rule_timeout_control = settings.allow_rule_timeout_control;
      timeout_threshold = settings.timeout_threshold;
      dataflow_traces = false;
      nosem = true;
      strict = false;
      matching_explanations = false;
      time_flag = false;
      inline_metavariables = false;
      engine_config = Engine_config.default;
    }
