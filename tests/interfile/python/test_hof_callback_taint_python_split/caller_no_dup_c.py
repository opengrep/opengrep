from caller_no_dup_b import caller_no_dup_b

def caller_no_dup_c():
    # ok: test-hof-callback-taint
    caller_no_dup_b()
