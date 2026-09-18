from propagates import propagates
from wrapper2 import wrapper2

def test_nested_tainted():
    # ruleid: test-hof-dedup
    sink(wrapper2(propagates, source()))


