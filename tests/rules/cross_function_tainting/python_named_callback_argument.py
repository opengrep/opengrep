def apply2(cb1, cb2, data):
    cb2(data)

def positional():
    # ruleid: python_named_callback_argument
    apply2(lambda y: None, lambda x: sink(x), source())

def keywords_in_order():
    # ruleid: python_named_callback_argument
    apply2(cb1=lambda y: None, cb2=lambda x: sink(x), data=source())

def keywords_reordered():
    # ruleid: python_named_callback_argument
    apply2(data=source(), cb2=lambda x: sink(x), cb1=lambda y: None)

def uncalled_callback_sinks():
    # ok: python_named_callback_argument
    apply2(cb2=lambda y: None, cb1=lambda x: sink(x), data=source())
