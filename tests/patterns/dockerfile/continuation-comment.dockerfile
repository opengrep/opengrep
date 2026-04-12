FROM alpine:3.19

# Comment inside a continuation: the RUN instruction should still match
# and the match should be reported at the RUN line (location correctness).

# MATCH:
RUN apt-get install -y \
# this comment is transparent to the instruction
    curl \
# another transparent comment
    wget

# Not a match (different command)
RUN echo done
