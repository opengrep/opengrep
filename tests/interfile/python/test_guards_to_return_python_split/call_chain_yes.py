from outer_yes import outer_yes
from sink import sink

def call_chain_yes():
    # ruleid: test-guards-to-return
    sink(outer_yes({"data": [1, 2]}))


