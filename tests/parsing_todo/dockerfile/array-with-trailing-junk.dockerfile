# 'docker build' JSON-parses the whole argument, so it treats these lines as
# shell commands: an array followed by anything else is not valid JSON. We
# stop validating at the closing bracket and commit to the exec form, which
# makes the trailing text a syntax error.
FROM alpine
RUN ["a"] && echo done
CMD ["a"] extra
