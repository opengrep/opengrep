FROM alpine:3.19

# MATCH:
LABEL key1=val1 \
    # a comment in continuation \
    key2=val2
