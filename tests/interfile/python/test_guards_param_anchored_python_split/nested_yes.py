def nested_yes(y, x):
    if y["field"]["k"]:
        # ruleid: test-guards-param-anchored
        sink(x)

