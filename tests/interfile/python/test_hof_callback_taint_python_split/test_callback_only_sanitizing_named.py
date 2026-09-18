from app_callback_only import app_callback_only
from sanitizes import sanitizes

def test_callback_only_sanitizing_named():
    # ok: test-hof-callback-taint
    return sink(app_callback_only(sanitizes, source()))

