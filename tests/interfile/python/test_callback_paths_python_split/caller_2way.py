from handler_2way_a import handler_2way_a
from handler_2way_b import handler_2way_b
from my_hof_2way import my_hof_2way
from source import source

def caller_2way():
    my_hof_2way(
        {"a": handler_2way_a, "b": handler_2way_b, "data": source()},
        True,
    )


