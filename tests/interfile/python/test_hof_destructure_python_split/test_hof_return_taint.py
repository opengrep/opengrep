from handler_passthrough import handler_passthrough
from my_hof import my_hof

def test_hof_return_taint():
    opts = {"cb": handler_passthrough, "data": source()}
    # ruleid: test-hof-destructure-taint
    sink(my_hof(opts))
