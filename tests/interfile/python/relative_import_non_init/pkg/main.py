# Non-init sibling: a regular .py inside the package directory.
# [from . import helpers] should bind [helpers] to qn [pkg.helpers].
# Buggy [module_name_string] resolves it to [pkg.main.helpers]
# instead — no such file — so [helpers.get_tainted] doesn't resolve
# and the cross-file taint flow is silently dropped.
from . import helpers


def sink(s):
    print(s)


def run():
    x = helpers.get_tainted()
    # ruleid: relative-import-non-init
    sink(x)
