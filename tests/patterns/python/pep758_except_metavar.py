try:
  pass
except ValueError, TypeError, RuntimeError:
  pass

try:
  pass
except (ValueError, TypeError):
  pass

# ERROR:
try:
  pass
except ValueError:
  pass
