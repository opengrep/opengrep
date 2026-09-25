# The `as` form is the only way to bind an exception name.
# The comma form is always parsed as a PEP 758 tuple.

# `as` bind -> match
# ERROR:
try:
  pass
except ValueError as e:
  pass

# Comma form: PEP 758 tuple, not a bind -> no match
try:
  pass
except ValueError, e:
  pass

# Three bare Names: PEP 758 tuple, not a bind -> no match
try:
  pass
except ValueError, TypeError, RuntimeError:
  pass

# Parenthesized tuple, no bind -> no match
try:
  pass
except (ValueError, TypeError):
  pass
