# escape=`
# With a backtick escape directive a backslash is an ordinary character, so
# bracket expressions containing backslashes must still parse.
FROM alpine:3.19

# MATCH:
RUN [ -f /etc/os-release ]

# MATCH:
RUN [ -f /etc/os-release ] `
    && echo yes

RUN grep 'Python [0-9]*\.[0-9]*' /etc/issue
RUN ["echo", "[ok]"]
