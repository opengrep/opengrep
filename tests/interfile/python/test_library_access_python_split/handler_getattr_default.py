def handler_getattr_default(obj):
    # ruleid: test-library-access-taint
    sink(getattr(obj, "body", source()))

