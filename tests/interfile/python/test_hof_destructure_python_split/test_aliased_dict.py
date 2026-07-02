def test_aliased_dict():
    opts = {"cb": handler_aliased, "data": source()}
    my_hof(opts)


