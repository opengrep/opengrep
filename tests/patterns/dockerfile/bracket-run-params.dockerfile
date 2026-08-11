FROM alpine

# MATCH:
RUN --network=none [ -f /etc/os-release ]

# MATCH:
RUN --mount=type=cache,target=/root/.cache [ -f /etc/os-release ]

RUN --mount=type=cache,target=/root/.cache ["echo", "hi"]
RUN --network=none ["echo", "hi"]
