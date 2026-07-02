def caller_3way():
    my_hof_3way(
        {
            "x": handler_3way_x,
            "y": handler_3way_y,
            "z": handler_3way_z,
            "data": source(),
        },
        "x",
    )


