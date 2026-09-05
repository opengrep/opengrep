# Decoy: a same-named [emit] elsewhere in the project, so resolving
# [emit(self.data)] to base.emit is real cross-file resolution, not a
# unique-name fallback.
def emit(x):
    pass
