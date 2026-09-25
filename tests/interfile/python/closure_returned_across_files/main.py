from closures import mk_local, mk_pair, source


def returned_over_local():
    f = mk_local()
    # ruleid: closure-returned-across-files
    sink(f())


def shared_local():
    add, get = mk_pair()
    add(source())
    # ruleid: closure-returned-across-files
    sink(get())


def shared_local_clean():
    add, get = mk_pair()
    add("clean")
    # ok: closure-returned-across-files
    sink(get())
