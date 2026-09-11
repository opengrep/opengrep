from helper import helper
from propagates import propagates

def test_third_tainted():
    # ruleid: test-hof-dedup
    sink(helper(propagates, 1, 1, source()))


