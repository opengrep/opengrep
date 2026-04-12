FROM alpine:3.19

# Issue #629: trailing whitespace after line-continuation backslash
# "\  " (backslash + spaces) at end of line used to cause a
# Tok.NoTokenLocation crash in concat_shell_fragments.
RUN echo hello \  
    world

RUN apt-get install -y \
    curl \
    wget
