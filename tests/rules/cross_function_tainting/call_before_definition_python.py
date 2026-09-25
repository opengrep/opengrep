# A module level statement runs in order: a call made before a later def
# statement calls the value the name holds at that point, here an integer.
handler = 0
handler(source())


def handler(a):
    # ok: call_before_definition_python
    sink(a)


checker = 0


def checker(a):
    # ruleid: call_before_definition_python
    sink(a)


checker(source())
