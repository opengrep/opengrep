FROM golang:1.14

# Legacy space-separated LABEL (no '=')
LABEL maintainer "Foo Bar <foobar@example.com>"

# LABEL key with colons (OCI reverse-DNS convention)
LABEL che:server:6080:ref=VNC che:server:6080:protocol=http

# Both in a realistic Dockerfile
ENV DEBIAN_FRONTEND noninteractive
ENV TERM xterm
ENV LANG en_GB.UTF-8
ENV PATH=$JAVA_HOME/bin:$M2_HOME/bin:$PATH
ENV ANDROID_HOME=/home/user/android-sdk-linux
