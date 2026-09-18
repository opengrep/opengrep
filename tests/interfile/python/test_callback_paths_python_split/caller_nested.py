from handler_a_leader import handler_a_leader
from handler_a_sub import handler_a_sub
from handler_b_leader import handler_b_leader
from handler_b_sub import handler_b_sub
from my_hof_nested import my_hof_nested
from source import source

def caller_nested():
    my_hof_nested(
        {
            "team_a": {"leader": handler_a_leader, "sub": handler_a_sub},
            "team_b": {"leader": handler_b_leader, "sub": handler_b_sub},
            "data": source(),
        },
        "team_a",
    )


