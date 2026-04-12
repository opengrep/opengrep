FROM alpine:3.19

# Trailing whitespace after backslash continuation (issue #629).
# The match must be reported at the RUN line.

# MATCH:
RUN echo hello \  
    world

# MATCH:
RUN echo a \\
    b

# MATCH:
RUN echo foo \\ 
    bar
