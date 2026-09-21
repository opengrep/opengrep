def poly(v, c):
    p, q = v if c else (v, "")
    # ruleid: python_join_parameter_with_object
    sink(q)


def clean(v, c):
    p, q = v if c else (v, "")
    # ok: python_join_parameter_with_object
    sink(q)


def caller(self):
    poly(self.path, True)
    clean("safe", True)
