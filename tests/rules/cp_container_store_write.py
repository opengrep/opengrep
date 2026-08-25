STORED = "abc"
STORED[0] = "x"

PLAIN = "abc"

# A store into a temporary literal does not write the variables inside it.
TEMP = "abc"
[TEMP][0] = "x"
(TEMP,)[0] = "x"


def use():
    # A subscript store counts as a write to STORED, so it is no longer
    # "assigned just once" and its initial value must not propagate.
    # ok: cp-container-store-write
    f(STORED)
    # ruleid: cp-container-store-write
    f(PLAIN)
    # ruleid: cp-container-store-write
    f(TEMP)
