FROM alpine

# MATCH:
LABEL a="[not, an, array]"

# MATCH:
LABEL a='["also", "not"]'

# MATCH:
LABEL a=[bare]

ENV E="[1, 2]"
ENV F=[3]
ARG G=[4]
WORKDIR /a[0]
USER root
EXPOSE 8080
COPY a[0] /dst
