import re

# ruleid: redos-raw-string
re.compile(r"^(’+)+$")

# ruleid: redos-raw-string
re.compile(r"^([’]+)+$")

# ok: redos-raw-string
re.compile(r"^[’]+$")
