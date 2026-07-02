def handler_getattr_pos(obj):
    # ruleid: test-library-access-taint
    sink(getattr(obj, "body"))

