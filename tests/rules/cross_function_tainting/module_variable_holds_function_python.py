# A call through a module variable inside a function reaches the function
# the variable holds when the call runs: any function assigned to it.
def first(x):
    # ruleid: module_variable_holds_function_python
    sink(x)


def second(x):
    # ruleid: module_variable_holds_function_python
    sink(x)


def unused(x):
    # ok: module_variable_holds_function_python
    sink(x)


handler = first
other = unused


def switch():
    global handler
    handler = second


def use():
    handler(source())
