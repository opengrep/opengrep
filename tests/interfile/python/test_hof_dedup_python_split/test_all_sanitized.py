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


