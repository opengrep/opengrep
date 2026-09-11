from outer_no import outer_no
from sink import sink

def call_chain_no():
    # ok: test-guards-to-return
    sink(outer_no({"data": [1]}))


