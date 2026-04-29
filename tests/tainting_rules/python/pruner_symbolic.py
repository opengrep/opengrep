# Pin the prunable surface: for each kind that
# [Dataflow_svalue.is_symbolic_expr] propagates, exercise a
# constant-folded condition the pruner should fold to dead. If
# someone narrows [is_symbolic_expr], the corresponding case here
# starts firing and the fixture fails.

def source(): return "taint"
def sink(_x): pass

# Literal Bool — folds directly without sym-prop.
def case_literal_bool():
    if False:
        # ok: pruner-symbolic
        sink(source())

# Name + svalue: a variable bound to a constant boolean propagates
# via [N] in [is_symbolic_expr].
def case_var_bool():
    flag = False
    if flag:
        # ok: pruner-symbolic
        sink(source())

# Sequence container (list literal) — propagates via [Container].
# [Eval_il_partial.eval] folds [length] over a known list literal.
def case_container_list_len():
    arr = [1]
    if len(arr) == 2:
        # ok: pruner-symbolic
        sink(source())

# Tuple container.
def case_container_tuple_len():
    arr = (1, 2)
    if len(arr) == 3:
        # ok: pruner-symbolic
        sink(source())

# Set container.
def case_container_set_len():
    s = {1, 2}
    if len(s) == 3:
        # ok: pruner-symbolic
        sink(source())

# Dict container.
def case_container_dict_len():
    d = {"a": 1}
    if len(d) == 2:
        # ok: pruner-symbolic
        sink(source())

# Live counterpart — the analysis still fires for non-folded
# conditions and for conditions that fold true.
def case_live_fires():
    if True:
        # ruleid: pruner-symbolic
        sink(source())
