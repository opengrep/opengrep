FROM alpine

# MATCH:
VOLUME ["/a"]

# MATCH:
VOLUME [ "/a" , "/b" ]

VOLUME /a /b
VOLUME /a[0]
SHELL ["powershell", "-command"]
HEALTHCHECK CMD ["curl", "-f", "http://x/"]
HEALTHCHECK --interval=5m CMD [ -f /tmp/ok ]
ONBUILD RUN [ -d /src ] && echo yes
ONBUILD RUN ["echo", "built"]
