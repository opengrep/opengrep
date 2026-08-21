def call_forward_yes():
    # ruleid: test-guards-hof-return
    sink(wrapper(True, source()))
