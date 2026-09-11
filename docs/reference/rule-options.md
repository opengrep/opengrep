# Rule options

<!-- BEGIN GENERATED: stamp -->
> Reference for **opengrep 1.30.0** (commit `d094c70bb`).
<!-- END GENERATED: stamp -->

The `options:` key of a rule tunes how that rule matches. Options apply to the
rule that sets them only.

```yaml
rules:
  - id: example
    pattern: ...
    options:
      symbolic_propagation: true
```

Opengrep ignores option names it does not know, without a warning, so a
misspelled option silently has no effect. Some options have an equivalent
command-line flag that enables the same behaviour for every rule. Such a flag
cannot switch an option off for a rule that enables it.

<!-- BEGIN GENERATED: index -->
| Name | Summary |
|---|---|
| [`dynamic_timeout`](rule-options/dynamic_timeout.md) | Scale this rule's time limit with the size of each file; needs --allow-rule-timeout-control. |
| [`symbolic_propagation`](rule-options/symbolic_propagation.md) | Let patterns see through variables that hold the result of an expression. |
| [`taint_interfile`](rule-options/taint_interfile.md) | Follow taint across files, through calls to functions defined in other files. |
| [`taint_intrafile`](rule-options/taint_intrafile.md) | Follow taint through calls to functions defined in the same file. |

Not yet documented (45): `ac_matching`, `arrow_is_function`, `attr_expr`, `commutative_boolop`, `commutative_compop`, `constant_propagation`, `cpp_parsing_pref`, `decorators_order_matters`, `dynamic_timeout_max_multiplier`, `dynamic_timeout_unit_kb`, `flddef_assign`, `generic_braces`, `generic_caseless`, `generic_comment_style`, `generic_ellipsis_max_span`, `generic_engine`, `generic_extra_braces`, `generic_extra_word_characters`, `generic_multiline`, `go_deeper_expr`, `go_deeper_stmt`, `guarded_taint_signatures`, `implicit_deep_exprstmt`, `implicit_ellipsis`, `implicit_return`, `let_is_var`, `max_match_per_file`, `symmetric_eq`, `taint_assume_safe_booleans`, `taint_assume_safe_comparisons`, `taint_assume_safe_functions`, `taint_assume_safe_indexes`, `taint_assume_safe_numbers`, `taint_fixpoint_timeout`, `taint_focus_on`, `taint_interfile_depth`, `taint_match_on`, `taint_only_propagate_through_assignments`, `taint_unify_mvars`, `timeout`, `unify_ids_strictly`, `vardef_assign`, `xml_attrs_implicit_ellipsis`, `xml_children_ordered`, `xml_singleton_loose_matching`
<!-- END GENERATED: index -->
