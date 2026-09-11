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

An option name that opengrep does not know is an error: the run stops with
exit status 2 and reports `Found unknown JSON field <name>`. Note that this
differs from an unknown key elsewhere in a rule, which only produces a warning.

Some options have an equivalent command-line flag that enables the same
behaviour for every rule. Such a flag cannot switch an option off for a rule
that enables it.

<!-- BEGIN GENERATED: index -->
| Name | Summary |
|---|---|
| [`ac_matching`](rule-options/ac_matching.md) | Match chains of an associative operator as a flat list of operands. |
| [`arrow_is_function`](rule-options/arrow_is_function.md) | Match arrow functions and function expressions like other functions. |
| [`attr_expr`](rule-options/attr_expr.md) | Let a call pattern match a decorator or an annotation written like a call. |
| [`commutative_boolop`](rule-options/commutative_boolop.md) | Match the operands of && and || in any order. |
| [`constant_propagation`](rule-options/constant_propagation.md) | Let a literal in a pattern match a variable known to hold that value. |
| [`cpp_parsing_pref`](rule-options/cpp_parsing_pref.md) | Read an ambiguous C++ pattern such as foo $X(...); as a function declaration or a constructed variable. |
| [`decorators_order_matters`](rule-options/decorators_order_matters.md) | Require decorators and annotations to appear in the order the pattern gives. |
| [`dynamic_timeout`](rule-options/dynamic_timeout.md) | Scale this rule's time limit with the size of each file; needs --allow-rule-timeout-control. |
| [`dynamic_timeout_max_multiplier`](rule-options/dynamic_timeout_max_multiplier.md) | For this rule, the most that file size may multiply its time limit; needs --allow-rule-timeout-control. |
| [`dynamic_timeout_unit_kb`](rule-options/dynamic_timeout_unit_kb.md) | For this rule, how many KB of file earn one more time limit; needs --allow-rule-timeout-control. |
| [`flddef_assign`](rule-options/flddef_assign.md) | Let a pattern assigning a function match methods and fields that hold functions. |
| [`generic_braces`](rule-options/generic_braces.md) | With aliengrep, replace the pairs of braces that an ellipsis keeps balanced. |
| [`generic_caseless`](rule-options/generic_caseless.md) | With aliengrep, match text regardless of letter case. |
| [`generic_comment_style`](rule-options/generic_comment_style.md) | With spacegrep, remove comments of one style from the target before matching. |
| [`generic_ellipsis_max_span`](rule-options/generic_ellipsis_max_span.md) | With spacegrep, the most line breaks an ellipsis may span. |
| [`generic_engine`](rule-options/generic_engine.md) | Choose the engine behind languages: [generic], spacegrep or aliengrep. |
| [`generic_extra_braces`](rule-options/generic_extra_braces.md) | With aliengrep, add pairs of braces that an ellipsis keeps balanced. |
| [`generic_extra_word_characters`](rule-options/generic_extra_word_characters.md) | With aliengrep, add characters that a metavariable may capture as part of a word. |
| [`generic_multiline`](rule-options/generic_multiline.md) | With aliengrep, whether an ellipsis may span lines; false keeps it within one. |
| [`go_deeper_expr`](rule-options/go_deeper_expr.md) | Let deep expression patterns and statements look inside expressions. |
| [`go_deeper_stmt`](rule-options/go_deeper_stmt.md) | Let ... between statements reach statements nested in later blocks. |
| [`guarded_taint_signatures`](rule-options/guarded_taint_signatures.md) | Drop a cross-function taint finding when the branch leading to the sink cannot be taken. |
| [`implicit_deep_exprstmt`](rule-options/implicit_deep_exprstmt.md) | Let an expression statement in a statement pattern match a statement containing it. |
| [`implicit_ellipsis`](rule-options/implicit_ellipsis.md) | Let a record or class pattern match targets with more fields than it lists. |
| [`implicit_return`](rule-options/implicit_return.md) | Let a return pattern match the value a function returns without the return keyword. |
| [`let_is_var`](rule-options/let_is_var.md) | Let a var declaration pattern match let and const declarations. |
| [`max_match_per_file`](rule-options/max_match_per_file.md) | Keep only the first findings of this rule in each file. |
| [`symbolic_propagation`](rule-options/symbolic_propagation.md) | Let patterns see through variables that hold the result of an expression. |
| [`symmetric_eq`](rule-options/symmetric_eq.md) | Match the operands of == and != in either order. |
| [`taint_assume_safe_booleans`](rule-options/taint_assume_safe_booleans.md) | Assume that a boolean value is clean, even when it was computed from tainted data. |
| [`taint_assume_safe_comparisons`](rule-options/taint_assume_safe_comparisons.md) | Assume that the result of a comparison is clean, even when an operand is tainted. |
| [`taint_assume_safe_functions`](rule-options/taint_assume_safe_functions.md) | Assume that a call returns clean data, even when its arguments are tainted. |
| [`taint_assume_safe_indexes`](rule-options/taint_assume_safe_indexes.md) | Assume that a tainted index does not taint the element it selects. |
| [`taint_assume_safe_numbers`](rule-options/taint_assume_safe_numbers.md) | Assume that a number is clean, even when it was computed from tainted data. |
| [`taint_fixpoint_timeout`](rule-options/taint_fixpoint_timeout.md) | Meant to limit the time spent on the taint analysis of one function; has no effect. |
| [`taint_focus_on`](rule-options/taint_focus_on.md) | Report a taint finding at the source instead of at the sink. |
| [`taint_interfile`](rule-options/taint_interfile.md) | Follow taint across files, through calls to functions defined in other files. |
| [`taint_interfile_depth`](rule-options/taint_interfile_depth.md) | How many calls deep this rule's cross-file taint analysis follows a chain. |
| [`taint_intrafile`](rule-options/taint_intrafile.md) | Follow taint through calls to functions defined in the same file. |
| [`taint_only_propagate_through_assignments`](rule-options/taint_only_propagate_through_assignments.md) | Let taint move only by assignment, not through operators or calls. |
| [`taint_unify_mvars`](rule-options/taint_unify_mvars.md) | Require a metavariable used in both a source and a sink to bind the same code in both. |
| [`timeout`](rule-options/timeout.md) | This rule's time limit on each file, in seconds; needs --allow-rule-timeout-control. |
| [`unify_ids_strictly`](rule-options/unify_ids_strictly.md) | Require a metavariable bound twice to name the same variable, not just the same name. |
| [`vardef_assign`](rule-options/vardef_assign.md) | Let an assignment pattern match a variable declaration with an initial value. |
| [`xml_attrs_implicit_ellipsis`](rule-options/xml_attrs_implicit_ellipsis.md) | Let an XML pattern match elements with more attributes than it lists. |
| [`xml_children_ordered`](rule-options/xml_children_ordered.md) | Require the children of an XML element to appear in the order the pattern gives. |
| [`xml_singleton_loose_matching`](rule-options/xml_singleton_loose_matching.md) | Let a self-closing element and an element with open and close tags match each other. |
<!-- END GENERATED: index -->

## Examples

### A misspelled option stops the run

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: unsafe-query
    pattern: $DB.cursor().execute(...)
    message: raw SQL executed on a cursor
    languages: [python]
    severity: WARNING
    options:
      symbolic_propagatio: true
```

**`app.py`**
```python title="app.py"
def query(db, sql):
    cur = db.cursor()
    cur.execute(sql)
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml app.py 2>&1 | sed 's/ while expecting.*//'
[00.04][ERROR]: Error: Found unknown JSON field symbolic_propagatio
$ opengrep scan --config rule.yaml app.py > /dev/null 2>&1; echo "exit status: $?"
exit status: 2
```

The rest of the message names the type opengrep expected, in its own
`Rule_options.atd`.
