FROM golang:1.21

# ARG before FROM is tested in arg-before-from.dockerfile

# Simple spaced ENV (single-line, existing behaviour)
ENV GOARM 7

# Multiline ENV: value starts on the continuation lines
ENV KUBE_DYNAMIC_CROSSPLATFORMS \
  arm64 \
  mips64el

ENV KUBE_CROSSPLATFORMS \
  linux/arm64 \
  linux/mips64le

# Multiline ENV: value starts on the same line and continues
ENV PATH /usr/local/go/bin \
  /usr/local/bin \
  /usr/bin
