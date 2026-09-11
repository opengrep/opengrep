from outer3_yes import outer3_yes
from sink import sink

def call_chain3_yes():
    # ruleid: test-guards-to-return
    sink(outer3_yes({"data": [1, 2]}, "f"))
