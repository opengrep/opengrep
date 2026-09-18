from propagates import propagates
from wrapper2 import wrapper2

def test_nested_clean():
    # ok: test-hof-dedup
    sink(wrapper2(propagates, 1))
