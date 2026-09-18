# Exercises dedup_sig_effects + recursive_cache in Sig_inst.ml.
#
# [helper] holds a signature with three symbolic taint paths
# (one per argument).  Under the current exact-match dedup these paths
# stay separate and each argument's tainted-or-not status is preserved
# at instantiation time.  A regression to merging-based dedup would
# collapse them and lose per-argument granularity.
#
# The nested [wrapper] / [wrapper2] chain exercises recursive_cache:
# without memoisation the inner [helper] signature would be
# re-instantiated for every outer effect.


def propagates(x):
    return x


def sanitize(x):
    return 0


def helper(cb, a, b, c):
    return cb(a) + cb(b) + cb(c)


# === Exact-match dedup: each arg independently tracked ===


def test_first_tainted():
    # ruleid: test-hof-dedup
    sink(helper(propagates, source(), 1, 1))


def test_second_tainted():
    # ruleid: test-hof-dedup
    sink(helper(propagates, 1, source(), 1))


def test_third_tainted():
    # ruleid: test-hof-dedup
    sink(helper(propagates, 1, 1, source()))


def test_all_tainted():
    # ruleid: test-hof-dedup
    sink(helper(propagates, source(), source(), source()))


def test_none_tainted():
    # ok: test-hof-dedup
    sink(helper(propagates, 1, 1, 1))


def test_all_sanitized():
    # ok: test-hof-dedup
    sink(
        helper(
            propagates,
            sanitize(source()),
            sanitize(source()),
            sanitize(source()),
        )
    )


# === Recursive cache: same callback through nested HOFs ===


def wrapper(cb, x):
    return helper(cb, x, x, x)


def wrapper2(cb, x):
    return wrapper(cb, x)


def test_nested_tainted():
    # ruleid: test-hof-dedup
    sink(wrapper2(propagates, source()))


def test_nested_clean():
    # ok: test-hof-dedup
    sink(wrapper2(propagates, 1))
