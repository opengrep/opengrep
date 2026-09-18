from handler_direct import handler_direct
from my_hof import my_hof

def test_direct_dict():
    my_hof({"cb": handler_direct, "data": source()})


