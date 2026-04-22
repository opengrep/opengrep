# Test HOF taint propagation when the callback and data are reached via
# record/dict-field access on a parameter. Exercises:
#   - Symbolic propagation of dict literals (Dataflow_svalue).
#   - Arg-shape extension with Ofld/Ostr offsets (Taint_shape).
#   - resolve_callee_expr's RecordOrDict + svalue-walk paths at Sig_inst.
#   - Call-graph extraction of callbacks nested inside record/dict args
#     (Graph_from_AST.extract_callbacks_from_arg).


def my_hof(opts):
    cb = opts["cb"]
    data = opts["data"]
    return cb(data)


def handler(x):
    # ruleid: test-hof-destructure-taint
    sink(x)
    return x


# Direct dict literal at the call site.
def test_direct_dict():
    my_hof({"cb": handler, "data": source()})


# Dict literal bound to a variable, then passed — svalue walk recovers
# the callback through the alias.
def test_aliased_dict():
    opts = {"cb": handler, "data": source()}
    my_hof(opts)


# Wrapped in sink() at the caller so the finding also flows through
# my_hof's return in addition to firing at handler's sink.
def test_alias_with_wrapping_sink():
    opts = {"cb": handler, "data": source()}
    # ruleid: test-hof-destructure-taint
    sink(my_hof(opts))
