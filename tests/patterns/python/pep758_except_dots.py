# ERROR:
try:
  pass
except ValueError, TypeError, RuntimeError:
  pass

# ERROR:
try:
  pass
except (ValueError, TypeError):
  pass

# ERROR:
try:
  pass
except ValueError:
  pass
