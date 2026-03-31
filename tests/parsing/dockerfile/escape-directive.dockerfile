# escape=`
FROM alpine:3.19

# Issue #629: backtick as alternative escape/continuation character.
# The "# escape=`" directive tells Docker (and our preprocessor) to treat
# backtick as the line-continuation marker instead of backslash.
RUN echo hello `
    world

RUN apt-get install -y `
    curl `
    wget
