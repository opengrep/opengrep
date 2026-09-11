from helper import helper
from propagates import propagates
from sanitize import sanitize

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


