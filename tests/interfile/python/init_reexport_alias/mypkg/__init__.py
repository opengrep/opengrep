# Aliased package-init re-export: `mypkg.run` aliases
# `mypkg.processor.process`. The bound name (run) differs from the
# target name (process).
from .processor import process as run
