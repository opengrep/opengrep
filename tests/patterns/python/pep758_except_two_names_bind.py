# `except A, B:` with two bare Names is the Python 2 bind form.
# The parser treats this as: type=A, name=B (not as a tuple).
# So `except $T as $N:` SHOULD match it (it's a bind).

# ERROR:
try:
  pass
except ValueError, e:
  pass

# ERROR:
try:
  pass
except ValueError as e:
  pass

# Three bare Names: PEP 758 tuple, no bind -> no match
try:
  pass
except ValueError, TypeError, RuntimeError:
  pass

# Parenthesized tuple, no bind -> no match
try:
  pass
except (ValueError, TypeError):
  pass
