# ERROR:
try:
  pass
except ValueError, TypeError, RuntimeError:
  pass

# ERROR:
try:
  pass
except TypeError, ValueError, RuntimeError:
  pass

# ERROR:
try:
  pass
except (ValueError, TypeError):
  pass

try:
  pass
except TypeError, RuntimeError, OSError:
  pass

try:
  pass
except TypeError:
  pass
