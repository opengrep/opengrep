FROM alpine:3.19

# Issue #629: double backslash at end of continuation line.
# Docker treats the last backslash as the continuation marker regardless
# of what precedes it, so "echo a \\ " still continues to the next line.
RUN echo a \\
    world

# Same with trailing space after the double backslash
RUN echo hello \\ 
    world

RUN apt-get install -y \\
    curl
