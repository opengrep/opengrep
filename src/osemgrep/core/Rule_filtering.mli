type conf = {
  exclude_rule_ids : Rule_ID.t list;
  severity : Rule.severity list;
  exclude_products : Semgrep_output_v1_t.product list;
}
[@@deriving show]

val get_rule_product_from_metadata : Rule.rule -> Semgrep_output_v1_t.product

(* the rules that run: those of the severities of --severity, or by default
 * of every severity but Inventory and Experiment, minus the excluded rule
 * ids and products *)
val filter_rules : conf -> Rule.rules -> Rule.rules

(* the INVENTORY and EXPERIMENT rules left out by [filter_rules]; their
 * targets are still scanned, by no rule *)
val rules_not_run : conf -> Rule.rules -> Rule.rules
