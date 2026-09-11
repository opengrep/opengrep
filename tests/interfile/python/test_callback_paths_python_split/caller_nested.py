def caller_nested():
    my_hof_nested(
        {
            "team_a": {"leader": handler_a_leader, "sub": handler_a_sub},
            "team_b": {"leader": handler_b_leader, "sub": handler_b_sub},
            "data": source(),
        },
        "team_a",
    )


