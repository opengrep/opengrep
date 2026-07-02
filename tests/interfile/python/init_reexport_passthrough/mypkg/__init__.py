# Package-init re-export.  `from .processor import process` makes
# `mypkg.process` an alias for `mypkg.processor.process`.  Without
# init-file re-export tracking in projidx, callers writing
# `from mypkg import process` would fail to resolve and the call
# edge to processor.py would be missing.
from .processor import process
