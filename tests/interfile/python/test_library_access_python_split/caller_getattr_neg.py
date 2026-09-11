from handler_getattr_neg import handler_getattr_neg

def caller_getattr_neg():
    handler_getattr_neg({"body": "safe", "user": source()})

