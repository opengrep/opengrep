from handler_same import handler_same
from my_hof_same import my_hof_same
from source import source

def caller_same():
    my_hof_same({"a": handler_same, "data": source()}, True)


