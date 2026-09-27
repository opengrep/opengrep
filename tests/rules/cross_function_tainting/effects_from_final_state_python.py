g = None


def set_global(x):
    global g
    g = x


def writes_global_in_loop():
    x = "safe"
    for i in range(3):
        set_global(x)
        x = source()


def reads_global_after_call():
    writes_global_in_loop()
    # ruleid: effects_from_final_state_python
    sink(g)


def calls_callback_in_loop(cb):
    x = "safe"
    for i in range(3):
        cb(x)
        x = source()


def dangerous(v):
    # ruleid: effects_from_final_state_python
    sink(v)


def passes_callback():
    calls_callback_in_loop(dangerous)


def sinks_in_loop(y):
    x = "safe"
    for i in range(3):
        # ruleid: effects_from_final_state_python
        sink(x)
        x = y


def passes_source_to_loop():
    sinks_in_loop(source())


def cleans_before_loop_exit():
    x = source()
    for i in range(3):
        x = "safe"
        # ok: effects_from_final_state_python
        sink(x)
