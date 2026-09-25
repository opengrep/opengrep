from handlers import (
    get_handler,
    sink_through_dict,
    sink_through_variable,
    source,
)


def via_variable():
    f = sink_through_variable
    f(source())


def via_dict():
    handlers = {"k": sink_through_dict}
    handlers["k"](source())


def via_return():
    f = get_handler()
    f(source())


def sink_shadowed_by_local(a):
    # ok: function-reference-across-files
    sink(a)


def local_value_shadows_function():
    sink_shadowed_by_local = 0
    f = sink_shadowed_by_local
    f(source())
