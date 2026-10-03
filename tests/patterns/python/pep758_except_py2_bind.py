# ERROR:
try:
  pass
except ValueError as e:
  pass

try:
  pass
except ValueError, TypeError, RuntimeError:
  pass

try:
  pass
except (ValueError, TypeError):
  pass
