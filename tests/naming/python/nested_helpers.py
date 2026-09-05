def first(x):
    def helper(v):
        return v + 1

    return helper(x)


def second(x):
    def helper(v):
        return v + 2

    return helper(x)
