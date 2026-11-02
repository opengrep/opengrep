import re

# ruleid: redos-comment-group
re.compile("^(a+)+(?#a(b)x$")

# ok: redos-comment-group
re.compile("^a+(?#a(b)x$")
