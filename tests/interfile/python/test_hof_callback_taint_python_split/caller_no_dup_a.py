from direct_source_to_sink import direct_source_to_sink

def caller_no_dup_a():
    # ok: test-hof-callback-taint
    direct_source_to_sink()

