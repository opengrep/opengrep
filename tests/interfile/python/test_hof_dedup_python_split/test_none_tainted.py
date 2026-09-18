from helper import helper
from propagates import propagates

def test_none_tainted():
    # ok: test-hof-dedup
    sink(helper(propagates, 1, 1, 1))


