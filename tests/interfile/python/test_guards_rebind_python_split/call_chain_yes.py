from outer_yes import outer_yes
from source import source

def call_chain_yes():
    outer_yes("dummy", {"data": [1, 2]}, "dummy", source())


