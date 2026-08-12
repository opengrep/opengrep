# Shell-form RUN commands containing '[' must not be mistaken for exec-form
# arrays, matching what 'docker build' does.
FROM alpine:3.19
RUN [ -f /etc/os-release ]
RUN [ ! -d /var/cache ] && echo missing
RUN [ "$FOO" = "$BAR" ]
RUN grep 'Python [0-9]*\.[0-9]*'
RUN echo a[0]b
RUN awk '{print $1}' | sed 's/[a-z]//g'
RUN if [ -x /bin/sh ]; then echo y; fi
RUN foo \
    [ -f x ]
# Not valid JSON, so shell form for docker too.
CMD [ "a" "b" ]
ENTRYPOINT [ "a", ]
# A bracket the scanner has to give up on at end of line or end of input.
RUN ls [
RUN echo "]" "[" '[' ']'
RUN [
RUN [[ -f /x ]]
RUN [ "a
