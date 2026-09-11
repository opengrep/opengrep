from handler_two_of_three_a import handler_two_of_three_a
from handler_two_of_three_b import handler_two_of_three_b
from handler_unrelated import handler_unrelated
from my_hof_two_of_three import my_hof_two_of_three
from source import source

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
