# Each test case exercises one property only. Every test has its own
# handler (and its own sink line) so that findings cannot be deduplicated
# across flows; each ruleid annotation corresponds to a distinct taint path.


def my_hof(opts):
    cb = opts["cb"]
    data = opts["data"]
    return cb(data)


# ---------------------------------------------------------------------------
# (1) Direct dict literal at the call site.
#     Sig_inst walks the RecordOrDict step to locate the callback.
# ---------------------------------------------------------------------------

def handler_direct(x):
    # ruleid: test-hof-destructure-taint
    sink(x)


def test_direct_dict():
    my_hof({"cb": handler_direct, "data": source()})


# ---------------------------------------------------------------------------
# (2) Dict literal bound to a variable, then passed via the alias.
#     Sig_inst walks the variable's id_svalue to reach the dict and
#     locate the callback.
# ---------------------------------------------------------------------------

def handler_aliased(x):
    # ruleid: test-hof-destructure-taint
    sink(x)


def test_aliased_dict():
    opts = {"cb": handler_aliased, "data": source()}
    my_hof(opts)


# ---------------------------------------------------------------------------
# (3) HOF return value carries the callback's return taint.
#     The handler does not sink; the caller does, via my_hof's return.
# ---------------------------------------------------------------------------

def handler_passthrough(x):
    return x


def test_hof_return_taint():
    opts = {"cb": handler_passthrough, "data": source()}
    # ruleid: test-hof-destructure-taint
    sink(my_hof(opts))
