# MATCH:
ADD --link src dst

# MATCH:
ADD --link repo /app

ADD src dst

ADD --chown=someone src dst
