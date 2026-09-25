# A function defined inside a function binds its name in the enclosing
# function's scope, which a call in that scope sees before the module scope.
def helper(a):
    # ok: nested_function_shadows_module_function_python
    sink(a)


def outer():
    def helper(a):
        # ruleid: nested_function_shadows_module_function_python
        sink(a)

    helper(source())


def outer_by_reference():
    def helper(a):
        # ruleid: nested_function_shadows_module_function_python
        sink(a)

    def inner():
        f = helper
        f(source())

    inner()
