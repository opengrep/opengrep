# Two nested helpers of the same name are two bindings: no match.
def first():
    def helper():
        return 1
    return helper()


def second():
    def helper():
        return 2
    return helper()
