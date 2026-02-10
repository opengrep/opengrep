# MATCH:
COPY --chmod=755 --link src dst

COPY --link src dst

COPY --chmod=755 src dst

COPY src dst
