# Like 'docker build', which JSON-parses the whole argument: an array trailed
# by anything else is not valid JSON, so the line is a shell command.
FROM alpine
RUN ["a"] && echo done
CMD ["a"] extra
ENTRYPOINT ["a"] ; echo b
RUN ["a"] \
    && echo b
RUN ["a", \
     "b"] && echo c
# Only trailing blanks or a continuation: still the exec form.
RUN ["a"]	
ENTRYPOINT ["a"] \

CMD ["a"]
