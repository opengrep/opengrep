FROM alpine:3.19

# LABEL with comment in continuation (trailing backslash on comment)
LABEL key1=val1 \
    #maintainer="team@example.com" \
    key2=val2

# LABEL with multiple comments in continuation
LABEL x=1 \
    # comment one \
    # comment two \
    y=2

USER appuser
