# MATCH:
COPY --parents src dst

# MATCH:
COPY --parents /app/src /out

COPY src dst

COPY --link src dst
