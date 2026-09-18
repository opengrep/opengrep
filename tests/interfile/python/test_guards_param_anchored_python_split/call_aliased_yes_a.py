from aliased_yes import aliased_yes
from source import source

def call_aliased_yes_a():
    opts = {"data": [1, 2, 3]}
    aliased_yes(opts, source())

