FROM alpine:3.19

# Comment inside a RUN continuation: the comment is transparent and the
# result is equivalent to "RUN echo hello world".
RUN echo hello \
# comment between continuation lines
    world

# Multiple consecutive comments inside a continuation
RUN apt-get install -y \
# first comment
# second comment
    curl \
# third comment
    wget

# Comment at the start, not inside a continuation (should parse as blank)
# regular comment here
RUN echo done
