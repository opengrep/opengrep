def test_hof_return_taint():
    opts = {"cb": handler_passthrough, "data": source()}
    # ruleid: test-hof-destructure-taint
    sink(my_hof(opts))
