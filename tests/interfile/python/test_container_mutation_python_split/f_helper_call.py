def f_helper_call():
    arr = [1]
    grow(arr)
    if len(arr) == 2:
        # ruleid: container-mutation-prune
        sink(source())


