FROM alpine

# MATCH:
RUN echo [a]

# A '[' opening a fragment after a continuation is never an array.
# MATCH:
RUN echo \
    [b]

# MATCH:
RUN \
    echo [c]

# MATCH:
RUN echo [d] \
    && echo [e]

# MATCH:
RUN [ -f /x ] \
    && echo [f]

# Brackets on both sides of the continuation, and a continuation inside a
# bracketed word. These parse; the reinjected continuation splits the word
# into separate shell arguments, which is pre-existing behaviour unrelated
# to brackets ('echo ab\<newline>cd' behaves the same way).
RUN echo [g\
]
