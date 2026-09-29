def same_constant_in_loop():
    x = "a"
    while c():
        x = "a"
    # ruleid: constprop-loop-head-python
    sink(x)

def other_constant_in_loop():
    x = "a"
    while c():
        x = "b"
    # ok: constprop-loop-head-python
    sink(x)

def constant_before_loop():
    x = "a"
    while c():
        f()
    # todoruleid: constprop-loop-head-python
    sink(x)
