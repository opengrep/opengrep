from app_callback_only import app_callback_only
from propagates import propagates

def test_callback_only_propagating_named():
    # ruleid: test-hof-callback-taint
    return sink(app_callback_only(propagates, source()))

