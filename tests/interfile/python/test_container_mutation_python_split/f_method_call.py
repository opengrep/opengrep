from sink import sink
from source import source

def f_method_call():
    arr = [1]
    arr.append(2)
    if len(arr) == 2:
        # ruleid: container-mutation-prune
        sink(source())


