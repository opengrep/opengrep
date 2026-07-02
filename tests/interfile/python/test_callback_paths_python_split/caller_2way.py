def caller_2way():
    my_hof_2way(
        {"a": handler_2way_a, "b": handler_2way_b, "data": source()},
        True,
    )


