from helper import helper
from propagates import propagates

def test_second_tainted():
    # ruleid: test-hof-dedup
    sink(helper(propagates, 1, source(), 1))


