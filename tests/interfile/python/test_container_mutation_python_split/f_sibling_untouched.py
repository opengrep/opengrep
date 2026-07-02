def f_sibling_untouched():
    a = [1]
    b = [1, 2]
    grow(a)
    if len(b) == 2:
        # ruleid: container-mutation-prune
        sink(source())


