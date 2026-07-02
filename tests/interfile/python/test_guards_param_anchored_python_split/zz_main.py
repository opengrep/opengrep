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


# ---------- Direct Fetch cond (Bool literal at call site) ----------

# ---------- Else branch (FalseNode wraps cond in Operator(Not, _)) ----------

# ---------- Direct equality cond (Operator(Eq, [Fetch p; Lit N])) ----------

# ---------- Length comparison on a path (literal dict at call site) ----------

# ---------- Nested path, two levels (literal dict at call site) ----------

# ---------- Aliased caller, single level (requires svalue walker) ----------

# ---------- Aliased caller, nested path (requires multi-level walker) ----------

