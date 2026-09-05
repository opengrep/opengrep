# Level 2 re-export.  `foo.bar.process` is an alias for the real
# function in foo.bar.impl.process — the chain's leaf.
from .impl import process
