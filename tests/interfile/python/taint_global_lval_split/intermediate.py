from state_module import foo_lval


def intermediate():
    foo_lval()  # Taint the global through a cross-file call
