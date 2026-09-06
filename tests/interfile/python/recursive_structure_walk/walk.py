# Modeled on a type resolver over a syntax tree: one function, six recursive
# calls on different fields of its argument, and a sink inside.
def walk(x):
    if x.var is not None:
        walk(x.var)
    if x.expr is not None:
        walk(x.expr)
    if x.left is not None:
        walk(x.left)
    if x.right is not None:
        walk(x.right)
    if x.dim is not None:
        walk(x.dim)
    if x.name is not None:
        walk(x.name)
    # ruleid: recursive-structure-walk-taint
    sink(x.value)
