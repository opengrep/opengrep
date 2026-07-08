# Cross-file sink wrapper: the tainted [self.data] from app.py reaches the
# real sink here, so the finding requires interfile resolution of [emit].
def emit(x):
    # ruleid: this-field-write-via-self-call
    sink(x)
