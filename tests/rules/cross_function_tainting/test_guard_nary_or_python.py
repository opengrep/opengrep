# Python [a or b or c] lowers to one n-ary [__op_||__] call, which the
# partial evaluator cannot fold (it folds binary And/Or only). As a DNF
# guard the chain contributes one clause per disjunct, so a call that
# refutes every disjunct drops the effect; previously the whole chain was
# a single opaque atom and the refuted call still reported.


def source():
    return "taint"


def sink(_x):
    pass


def f_refuted(flag, x):
    if flag == 1 or flag == 2 or flag == 3:
        # ok: test-guard-nary-or
        sink(x)


def f_hit(flag, x):
    if flag == 1 or flag == 2 or flag == 3:
        # ruleid: test-guard-nary-or
        sink(x)


def calls():
    f_refuted(0, source())
    f_hit(2, source())
