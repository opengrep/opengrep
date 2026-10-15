# a pattern with a leading '::' asks for the top-level constant, so it
# matches only the name written with one

# MATCH:
::Foo.bar(2)

# OK: no leading '::'
Foo.bar(1)
