# Field-sensitive taint through Python's getattr library call. A
# source at the bound key fires; a source at a sibling key does not.
# The default-value form evaluates the default eagerly and its taint
# flows into the result via the conditional.
#
# [d.get("k")] is NOT rewritten here because [.get] is overloaded
# across Python's stdlib / third-party libraries and without type
# info we cannot tell a dict receiver apart from a method on some
# other object.

def handler_getattr_pos(obj):
    # ruleid: test-library-access-taint
    sink(getattr(obj, "body"))

def caller_getattr_pos():
    handler_getattr_pos({"body": source(), "user": "safe"})

def handler_getattr_neg(obj):
    # ok: test-library-access-taint
    sink(getattr(obj, "body"))

def caller_getattr_neg():
    handler_getattr_neg({"body": "safe", "user": source()})

def handler_getattr_default(obj):
    # ruleid: test-library-access-taint
    sink(getattr(obj, "body", source()))

def caller_getattr_default():
    handler_getattr_default({"body": "safe"})
