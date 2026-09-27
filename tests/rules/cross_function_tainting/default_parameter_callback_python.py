def tainted():
    return source()


def clean():
    return "safe"


def fetch(cb=tainted):
    return cb()


def no_default(cb):
    return cb()


def omits_the_argument():
    # ruleid: default_parameter_callback_python
    sink(fetch())


def passes_a_clean_function():
    # ok: default_parameter_callback_python
    sink(fetch(clean))


def omits_an_argument_with_no_default():
    # ok: default_parameter_callback_python
    sink(no_default())
