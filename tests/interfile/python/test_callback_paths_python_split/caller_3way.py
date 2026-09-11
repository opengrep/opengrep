from handler_3way_x import handler_3way_x
from handler_3way_y import handler_3way_y
from handler_3way_z import handler_3way_z
from my_hof_3way import my_hof_3way
from source import source

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


