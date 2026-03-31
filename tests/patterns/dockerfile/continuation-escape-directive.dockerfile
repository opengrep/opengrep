# escape=`
FROM alpine:3.19

# Backtick as continuation character (Windows/PowerShell Dockerfiles).
# The match must be reported at the RUN line.

# MATCH:
RUN echo hello `
    world

# MATCH:
RUN echo foo `
    bar `
    baz
