# Each test case exercises one property only. Every test has its own
# handler (and its own sink line) so that findings cannot be deduplicated
# across flows; each ruleid annotation corresponds to a distinct taint path.


# ---------------------------------------------------------------------------
# (1) Direct dict literal at the call site.
#     Sig_inst walks the RecordOrDict step to locate the callback.
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# (2) Dict literal bound to a variable, then passed via the alias.
#     Sig_inst walks the variable's id_svalue to reach the dict and
#     locate the callback.
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# (3) HOF return value carries the callback's return taint.
#     The handler does not sink; the caller does, via my_hof's return.
# ---------------------------------------------------------------------------

