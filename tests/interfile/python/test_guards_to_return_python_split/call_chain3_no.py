from outer3_no import outer3_no
from sink import sink

def call_chain3_no():
    # ok: test-guards-to-return
    sink(outer3_no({"data": [1]}, "f"))


