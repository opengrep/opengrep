# `except A, B:` with two bare Names is the Python 2 bind form.
# The parser treats it as: type=A, name=B (not a tuple of types).
# So `except (..., TypeError, ...):` should NOT match it.

# Two bare Names: Python 2 bind, NOT a tuple -> no match
try:
  pass
except ValueError, TypeError:
  pass

# Three bare Names: PEP 758 tuple -> match
# ERROR:
try:
  pass
except ValueError, TypeError, RuntimeError:
  pass

# Parenthesized tuple -> match
# ERROR:
try:
  pass
except (ValueError, TypeError):
  pass

# Dotted + bare Name: the bare Name after comma triggers Python 2 bind
# (type=socket.error, name=TypeError) -> no match
try:
  pass
except socket.error, TypeError:
  pass
