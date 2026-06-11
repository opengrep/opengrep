# A guard clause binding the same expression to two distinct constants is
# unsatisfiable: [a == 1 && a == 2] cannot hold for any input, so the sink
# effect is dropped at clause normalisation even though [a] is unknown at
# the call site (three-valued evaluation alone cannot refute it). The same
# check covers length atoms — [len(a) == 1 && len(a) == 2] is the shape a
# cross-arity fused clause takes. The control nest repeats the same atom,
# which stays satisfiable and reports.


def source():
    return "taint"


def sink(_x):
    pass


def unknown():
    return 5


def f_dead(a, x):
    if a == 1:
        if a == 2:
            # ok: test-guard-clause-consistency
            sink(x)


def f_dead_len(a, x):
    if len(a) == 1:
        if len(a) == 2:
            # ok: test-guard-clause-consistency
            sink(x)


def f_live(a, x):
    if a == 1:
        if a == 1:
            # ruleid: test-guard-clause-consistency
            sink(x)


def calls():
    f_dead(unknown(), source())
    f_dead_len(unknown(), source())
    f_live(unknown(), source())
