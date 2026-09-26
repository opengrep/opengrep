def one(uid, folder):
    return (uid, folder)


def run(f):
    f()


def scalar_called_by_variable():
    s = ""

    def f():
        nonlocal s
        s = source()

    f()
    # ruleid: closure_assignment_python
    sink(s)


def scalar_passed_to_call():
    s = ""

    def f():
        nonlocal s
        s = source()

    run(f)
    # ruleid: closure_assignment_python
    sink(s)


def scalar_read_and_written():
    s = ""

    def f():
        nonlocal s
        print(s)
        s = source()

    f()
    # ruleid: closure_assignment_python
    sink(s)


def returned_called_by_variable():
    e = None

    def f():
        nonlocal e
        e = one(source(), 3)

    f()
    # ruleid: closure_assignment_python
    sink(e[0])
    # ok: closure_assignment_python
    sink(e[1])


def returned_read_and_written():
    e = None

    def f():
        nonlocal e
        print(e)
        e = one(source(), 3)

    f()
    # ruleid: closure_assignment_python
    sink(e[0])
    # ok: closure_assignment_python
    sink(e[1])


def mutated_called_by_variable():
    box = []

    def f():
        box.append(source())

    f()
    # ruleid: closure_assignment_python
    sink(box)


def not_called():
    s = ""

    def f():
        nonlocal s
        s = source()

    # ok: closure_assignment_python
    sink(s)


def assigned_without_nonlocal():
    s = ""

    def f():
        s = source()
        return s

    f()
    # ok: closure_assignment_python
    sink(s)


def assigned_without_nonlocal_passed_to_call():
    s = ""

    def f():
        s = source()
        return s

    run(f)
    # ok: closure_assignment_python
    sink(s)
