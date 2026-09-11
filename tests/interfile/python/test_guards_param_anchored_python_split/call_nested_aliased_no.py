from nested_aliased_no import nested_aliased_no
from source import source

def call_nested_aliased_no():
    y = {"field": {"k": False}}
    nested_aliased_no(y, source())


