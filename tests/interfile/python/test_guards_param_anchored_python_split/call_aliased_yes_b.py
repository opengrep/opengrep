from aliased_yes import aliased_yes
from source import source

def call_aliased_yes_b():
    opts = {"data": [1, 2]}
    aliased_yes(opts, source())


