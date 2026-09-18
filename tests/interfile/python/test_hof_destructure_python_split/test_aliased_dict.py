from handler_aliased import handler_aliased
from my_hof import my_hof

def test_aliased_dict():
    opts = {"cb": handler_aliased, "data": source()}
    my_hof(opts)


