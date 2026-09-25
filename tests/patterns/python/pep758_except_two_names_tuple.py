# `except A, B:` is always parsed as PEP 758 tuple (catch A or B).

# Two bare Names: PEP 758 tuple -> match
# ERROR:
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

# Dotted + bare Name: PEP 758 tuple -> match
# ERROR:
try:
  pass
except socket.error, TypeError:
  pass
