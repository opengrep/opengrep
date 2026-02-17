def foo(a, b, c):
    pass

# ruleid: metavar-comparison-multi-or
foo(1, -1, 1)

# ruleid: metavar-comparison-multi-or
foo(20, -1, 0)

# ruleid: metavar-comparison-multi-or
foo(20, 0, 1)

# ok: metavar-comparison-multi-or
foo(20, 0, 0)

# ok: metavar-comparison-multi-or
foo(16, 0, 0)
