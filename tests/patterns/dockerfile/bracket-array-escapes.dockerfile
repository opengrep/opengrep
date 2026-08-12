FROM alpine

# MATCH:
ENTRYPOINT ["éA￿ꯍ"]

# MATCH:
ENTRYPOINT ["\\", "\"", "\/"]

# Truncated \u escape: not valid JSON, so shell form.
ENTRYPOINT ["\u00"]

# Non-hex \u escape: not valid JSON, so shell form.
ENTRYPOINT ["\uZZZZ"]

# A backslash before a character JSON doesn't escape.
ENTRYPOINT ["\x41"]

# An unescaped quote closes the string, so the rest isn't valid JSON.
ENTRYPOINT ["a"b"]
