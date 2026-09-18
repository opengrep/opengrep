from app_callback_only import app_callback_only

def wrapper_propagates_callback(f, x):
    return app_callback_only(f, x)

