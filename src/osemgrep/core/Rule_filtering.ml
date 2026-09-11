module Out = Semgrep_output_v1_t

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Filtering rules

   Translated from exclude_rules.py and some code in formatter/base.py

   The rules of severity INVENTORY and EXPERIMENT never run: their findings
   are never output (pysemgrep ran the rules and dropped the findings).
   --severity selects the severities that run instead.
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type conf = {
  exclude_rule_ids : Rule_ID.t list;
  severity : Rule.severity list;
  exclude_products : Out.product list;
}
[@@deriving show]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let get_rule_product_from_metadata (rule : Rule.t) =
  match rule.metadata with
  | Some json -> (
      let product_field = JSON.member "product" json in
      let sca_field = JSON.member "sca-kind" json in
      match (product_field, sca_field) with
      | Some (String "secrets"), _ -> `Secrets
      | _, Some (String _) -> `SCA
      | _ -> `SAST)
  | _ -> `SAST

(* python: output.py DEFAULT_SHOWN_SEVERITIES *)
let scanned_severities (requested : Rule.severity list) : Rule.severity list =
  match requested with
  | [] -> [ `Info; `Low; `Warning; `Medium; `Error; `High; `Critical ]
  | severities -> severities

let has_scanned_severity (conf : conf) (r : Rule.t) : bool =
  List.exists (Rule.equal_severity r.severity) (scanned_severities conf.severity)

(* the rules that never run: INVENTORY and EXPERIMENT, unless asked for *)
let rules_not_run (conf : conf) (rules : Rule.rules) : Rule.rules =
  rules
  |> List.filter (fun (r : Rule.t) ->
         (match r.severity with
         | `Inventory
         | `Experiment ->
             true
         | _ -> false)
         && not (has_scanned_severity conf r))

let filter_rules (conf : conf) (rules : Rule.rules) : Rule.rules =
  rules
  |> List.filter (has_scanned_severity conf)
  |> List_.exclude (fun r -> List.mem (fst r.Rule.id) conf.exclude_rule_ids)
  |> List_.exclude (fun r ->
         List.mem (get_rule_product_from_metadata r) conf.exclude_products)
