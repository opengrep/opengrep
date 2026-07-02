# The module global and every function that reads/writes it live in this
# one module: Python's `global x` binds to the *current module's* global
# namespace, so a bare `global x` in a different file would be a distinct
# variable.  The interfile aspect is the call graph (main -> intermediate
# -> foo_lval, main -> do_sink), which spans files below.
x = None


def foo_lval():
    global x
    x = source1("taint")


def do_sink():
    global x
    # ruleid: test-global-lval-taint
    sink(x)
