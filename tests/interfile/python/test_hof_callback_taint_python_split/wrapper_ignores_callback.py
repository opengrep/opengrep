def wrapper_ignores_callback(callback, data):
    # `unknown_sanitizer` is NOT defined — no signature available.
    # The HOF's ToSinkInCall for arg 0 should be DROPPED, not
    # preserved with index=0 (which would alias to `callback`).
    return app_callback_only(unknown_sanitizer, data)

