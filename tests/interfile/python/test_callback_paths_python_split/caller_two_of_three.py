def caller_two_of_three():
    my_hof_two_of_three(
        {
            "a": handler_two_of_three_a,
            "b": handler_two_of_three_b,
            "c": handler_unrelated,
            "data": source(),
        },
        True,
    )
