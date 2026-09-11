from caller_no_dup_a import caller_no_dup_a

def caller_no_dup_b():
    # ok: test-hof-callback-taint
    caller_no_dup_a()

