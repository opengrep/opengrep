# Level 1 re-export.  `foo.process` is an alias for `foo.bar.process`,
# which is ITSELF a re-export (defined in bar/__init__.py).  The
# post-phase must chase the alias all the way to bar.impl.process,
# not stop at bar.process.
from .bar import process
