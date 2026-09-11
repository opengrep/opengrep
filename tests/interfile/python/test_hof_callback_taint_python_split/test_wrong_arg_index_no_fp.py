def test_wrong_arg_index_no_fp():
    # ok: test-hof-callback-taint
    # Without fix: ToSinkInCall preserved with arg index 0 → resolves
    # `propagates` as callback → FP.  With fix: dropped → correct.
    return sink(wrapper_ignores_callback(propagates, source()))

