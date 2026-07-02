def mid(tagger_fn, flag, val):
    # Forwards the guard parameter [flag] to the injected [tagger_fn]
    # without repackaging.  Taking [tagger_fn] as an argument keeps this
    # forwarding layer in its own module with no back-import of the state
    # module (no import cycle).
    tagger_fn(flag, val)
