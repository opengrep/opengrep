def poly(v, c):
    p, q = v if c else (v, "")
    # ruleid: python_join_parameter_with_object
    sink(q)


def clean(v, c):
    p, q = v if c else (v, "")
    # ok: python_join_parameter_with_object
    sink(q)


def field_clean(v, c):
    p, q = v if c else (v, "")
    # ruleid: python_join_parameter_with_object
    sink(p)
    # ok: python_join_parameter_with_object
    sink(q)


def field_tainted(v, c):
    p, q = v if c else (v, "")
    # ruleid: python_join_parameter_with_object
    sink(p)
    # ruleid: python_join_parameter_with_object
    sink(q)


def stored_leaf(v, u, c):
    w = foo(v)
    p, q = w if c else (u, "")
    # ok: python_join_parameter_with_object
    sink(q)


def stored_leaf_tainted(v, u, c):
    w = foo(v)
    p, q = w if c else (u, "")
    # ruleid: python_join_parameter_with_object
    sink(q)


def caller(self):
    poly(self.path, True)
    clean("safe", True)
    field_clean((self.path, ""), True)
    field_tainted(("", self.path), True)
    stored_leaf((self.path, ""), "safe", True)
    stored_leaf_tainted(("", self.path), "safe", True)
