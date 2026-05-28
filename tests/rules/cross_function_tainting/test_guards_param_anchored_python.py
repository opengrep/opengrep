# Parameter-anchored branch guards: at signature instantiation, effects
# recorded inside a branch whose condition depends on a callee parameter
# should be dropped when the caller's actual argument makes the condition
# evaluate to [G.Lit (G.Bool false)]. Each test case exercises one
# property only; every callee has its own sink line so findings cannot
# be deduplicated across flows.
#
# Taint originates at the caller via [source()] and flows into the
# callee as a parameter. The sink inside the callee takes that parameter.
# When every caller's actual makes the callee's branch condition
# definitively false, the ToSink effect in the callee's signature is
# dropped at instantiation and no finding is emitted.


def source():
    return "taint"


def sink(_x):
    pass


# ---------- Direct Fetch cond (Bool literal at call site) ----------

def bool_no(flag, x):
    if flag:
        # ok: test-guards-param-anchored
        sink(x)

def call_bool_no_a():
    bool_no(False, source())

def call_bool_no_b():
    bool_no(False, source())


def bool_yes(flag, x):
    if flag:
        # ruleid: test-guards-param-anchored
        sink(x)

def call_bool_yes_a():
    bool_yes(False, source())

def call_bool_yes_b():
    bool_yes(True, source())


# ---------- Else branch (FalseNode wraps cond in Operator(Not, _)) ----------

def else_no(flag, x):
    if flag:
        pass
    else:
        # ok: test-guards-param-anchored
        sink(x)

def call_else_no_a():
    else_no(True, source())

def call_else_no_b():
    else_no(True, source())


def else_yes(flag, x):
    if flag:
        pass
    else:
        # ruleid: test-guards-param-anchored
        sink(x)

def call_else_yes_a():
    else_yes(True, source())

def call_else_yes_b():
    else_yes(False, source())


# ---------- Direct equality cond (Operator(Eq, [Fetch p; Lit N])) ----------

def eq_no(code, x):
    if code == 0:
        # ok: test-guards-param-anchored
        sink(x)

def call_eq_no_a():
    eq_no(1, source())

def call_eq_no_b():
    eq_no(2, source())


def eq_yes(code, x):
    if code == 0:
        # ruleid: test-guards-param-anchored
        sink(x)

def call_eq_yes_a():
    eq_yes(1, source())

def call_eq_yes_b():
    eq_yes(0, source())


# ---------- Length comparison on a path (literal dict at call site) ----------

def lenpath_no(opts, x):
    if len(opts["data"]) == 2:
        # ok: test-guards-param-anchored
        sink(x)

def call_lenpath_no_a():
    lenpath_no({"data": [1]}, source())

def call_lenpath_no_b():
    lenpath_no({"data": [1, 2, 3]}, source())


def lenpath_yes(opts, x):
    if len(opts["data"]) == 2:
        # ruleid: test-guards-param-anchored
        sink(x)

def call_lenpath_yes_a():
    lenpath_yes({"data": [1]}, source())

def call_lenpath_yes_b():
    lenpath_yes({"data": [1, 2]}, source())


# ---------- Nested path, two levels (literal dict at call site) ----------

def nested_no(y, x):
    if y["field"]["k"]:
        # ok: test-guards-param-anchored
        sink(x)

def call_nested_no_a():
    nested_no({"field": {"k": False}}, source())

def call_nested_no_b():
    nested_no({"field": {"k": False}}, source())


def nested_yes(y, x):
    if y["field"]["k"]:
        # ruleid: test-guards-param-anchored
        sink(x)

def call_nested_yes_a():
    nested_yes({"field": {"k": False}}, source())

def call_nested_yes_b():
    nested_yes({"field": {"k": True}}, source())


# ---------- Aliased caller, single level (requires svalue walker) ----------

def aliased_no(opts, x):
    if len(opts["data"]) == 2:
        # ok: test-guards-param-anchored
        sink(x)

def call_aliased_no_a():
    opts = {"data": [1]}
    aliased_no(opts, source())

def call_aliased_no_b():
    opts = {"data": [1, 2, 3]}
    aliased_no(opts, source())


def aliased_yes(opts, x):
    if len(opts["data"]) == 2:
        # ruleid: test-guards-param-anchored
        sink(x)

def call_aliased_yes_a():
    opts = {"data": [1, 2, 3]}
    aliased_yes(opts, source())

def call_aliased_yes_b():
    opts = {"data": [1, 2]}
    aliased_yes(opts, source())


# ---------- Aliased caller, nested path (requires multi-level walker) ----------

def nested_aliased_no(y, x):
    if y["field"]["k"]:
        # ok: test-guards-param-anchored
        sink(x)

def call_nested_aliased_no():
    y = {"field": {"k": False}}
    nested_aliased_no(y, source())


def nested_aliased_yes(y, x):
    if y["field"]["k"]:
        # ruleid: test-guards-param-anchored
        sink(x)

def call_nested_aliased_yes_a():
    y = {"field": {"k": False}}
    nested_aliased_yes(y, source())

def call_nested_aliased_yes_b():
    y = {"field": {"k": True}}
    nested_aliased_yes(y, source())
