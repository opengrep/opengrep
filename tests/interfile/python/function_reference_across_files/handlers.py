def source():
    return ""


def sink_through_variable(a):
    # ruleid: function-reference-across-files
    sink(a)


def sink_through_dict(a):
    # ruleid: function-reference-across-files
    sink(a)


def sink_through_return(a):
    # ruleid: function-reference-across-files
    sink(a)


def get_handler():
    return sink_through_return
