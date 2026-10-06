# A built-in's placeholder for the data it maps over is not a parameter of
# the function calling it, whatever that parameter is called.
def handler(req, data):
    # ruleid: lambda-sig-py-builtin-data-param
    sink(list(map(str, source())))


def other(req, data):
    # ok: lambda-sig-py-builtin-data-param
    sink(list(map(str, req.args)))
