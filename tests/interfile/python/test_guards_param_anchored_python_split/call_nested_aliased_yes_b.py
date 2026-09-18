from nested_aliased_yes import nested_aliased_yes
from source import source

def call_nested_aliased_yes_b():
    y = {"field": {"k": True}}
    nested_aliased_yes(y, source())
