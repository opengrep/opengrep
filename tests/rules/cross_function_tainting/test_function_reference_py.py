def to_print(a):
    print(a)


def apply(cb, v):
    cb(v)


def sink_through_variable(a):
    # ruleid: test-function-reference-py
    sink(a)


def via_variable():
    f = sink_through_variable
    f(source())


def sink_through_dict(a):
    # ruleid: test-function-reference-py
    sink(a)


def via_dict():
    handlers = {"k": sink_through_dict}
    handlers["k"](source())


def sink_through_branch(a):
    # ruleid: test-function-reference-py
    sink(a)


def via_branch(c):
    f = sink_through_branch if c else to_print
    f(source())


def sink_through_return(a):
    # ruleid: test-function-reference-py
    sink(a)


def get_handler():
    return sink_through_return


def via_return():
    f = get_handler()
    f(source())


def sink_through_conditional_argument(a):
    # ruleid: test-function-reference-py
    sink(a)


def conditional_argument(c):
    apply(sink_through_conditional_argument if c else to_print, source())


def nested_function():
    def nested_sink(a):
        # ruleid: test-function-reference-py
        sink(a)

    f = nested_sink
    f(source())



def sink_shadowed_by_local(a):
    # ok: test-function-reference-py
    sink(a)


def local_value_shadows_function():
    sink_shadowed_by_local = 0
    f = sink_shadowed_by_local
    f(source())



def sink_shadowed_by_parameter_value(a):
    # ok: test-function-reference-py
    sink(a)


def local_bound_to_parameter(callback):
    sink_shadowed_by_parameter_value = callback
    f = sink_shadowed_by_parameter_value
    f(source())
