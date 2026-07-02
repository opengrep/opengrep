from hof import apply


def propagates(x):
    return x


def wrapper(callback, data):
    # `unknown_sanitizer` is NOT defined anywhere — no FuncDef, no
    # import.  fun_sig_opt for apply's `f` parameter at this call
    # site is None.  preserve_effect must DROP apply's ToSinkInCall
    # obligation; preserving it with the inner function's arg index
    # would re-key the effect against `wrapper`'s params, aliasing
    # to `callback` (param 0).
    return apply(unknown_sanitizer, data)


def test_no_fp():
    # ok: hof-unresolvable-callback-interfile
    # Without the A1 fix: wrapper's signature carries a phantom
    # ToSinkInCall on its `callback` param.  Calling it here with
    # callback=propagates would substitute propagates for the
    # callback alias, and the engine would conclude
    # `propagates(source())` → `sink(propagates(source()))` → FP.
    # With the fix: the effect is dropped at wrapper, no phantom
    # taint, no finding here.
    return sink(wrapper(propagates, source()))


def sink(x):
    print(x)


def source():
    return input()


test_no_fp()
