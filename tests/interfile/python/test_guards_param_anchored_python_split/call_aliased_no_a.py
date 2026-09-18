from aliased_no import aliased_no
from source import source

def call_aliased_no_a():
    opts = {"data": [1]}
    aliased_no(opts, source())

