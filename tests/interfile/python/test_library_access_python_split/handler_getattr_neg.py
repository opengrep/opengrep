def handler_getattr_neg(obj):
    # ok: test-library-access-taint
    sink(getattr(obj, "body"))

