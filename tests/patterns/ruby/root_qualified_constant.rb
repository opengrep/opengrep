# a leading '::' only says that a constant is looked up from the top level,
# so a pattern without one matches the name with or without it

# MATCH:
Foo.bar(1)

# MATCH:
::Foo.bar(2)

# OK: a constant nested in another module
Baz::Foo.bar(3)

# OK: a different constant
Bar.bar(4)
