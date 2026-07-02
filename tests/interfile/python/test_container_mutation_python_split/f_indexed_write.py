def f_indexed_write():
    arr = [1]
    arr[0] = 99
    if len(arr) == 1:
        # ruleid: container-mutation-prune
        sink(source())


