def call_nested_aliased_yes_b():
    y = {"field": {"k": True}}
    nested_aliased_yes(y, source())
