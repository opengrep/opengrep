from propagates import propagates
from wrapper_ignores_callback import wrapper_ignores_callback

def test_wrong_arg_index_no_fp():
    # Without fix: ToSinkInCall preserved with arg index 0 → resolves
    # `propagates` as callback → FP.  With fix: dropped → correct.
    # an unknown callee's result carries its arguments' taint
    # ruleid: test-hof-callback-taint
    return sink(wrapper_ignores_callback(propagates, source()))

