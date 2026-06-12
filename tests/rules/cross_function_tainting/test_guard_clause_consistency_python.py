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


# Escape-free distinct strings denote distinct runtime values: refute.
def f_dead_str(a, x):
    if a == "get":
        if a == "post":
            # ok: test-guard-clause-consistency
            sink(x)


# The Python parser unescapes string contents, so ["\n"] reaches the
# guard as a literal newline -- a runtime value with no backslash. The
# backslash abstention (insurance for parsers that store raw lexed
# contents) does not fire, and the refutation is value-correct.
def f_escape_refutes(a, x):
    if a == "\n":
        if a == "x":
            # ok: test-guard-clause-consistency
            sink(x)


# Same at evaluation: the substituted ["zzz" == "a\nb"] compares two
# runtime values and folds false, dropping the effect.
def g_escape_eval(s, x):
    if s == "a\nb":
        # ok: test-guard-clause-consistency
        sink(x)


def calls():
    f_dead(unknown(), source())
    f_dead_len(unknown(), source())
    f_live(unknown(), source())
    f_dead_str(unknown(), source())
    f_escape_refutes(unknown(), source())
    g_escape_eval("zzz", source())
