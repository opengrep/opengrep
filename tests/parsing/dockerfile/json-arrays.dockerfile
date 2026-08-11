# Exec-form arrays must keep parsing as arrays now that '[' can also start a
# shell fragment.
FROM alpine:3.19
RUN ["echo", "hello"]
CMD [ "a" , "b" ]
ENTRYPOINT []
VOLUME ["/a", "/b"]
SHELL ["powershell", "-command"]
RUN ["a\"b", "céd"]
CMD ["a", \
     "b"]
