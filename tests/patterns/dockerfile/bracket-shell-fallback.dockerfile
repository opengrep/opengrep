# A '[' that doesn't open a valid JSON array starts a shell command, the same
# way 'docker build' decides between the exec form and the shell form.
FROM alpine

# MATCH:
RUN [ -f /etc/os-release ]

# MATCH:
RUN [ "$FOO" = "$BAR" ]

# Missing comma.
# MATCH:
RUN ["a" "b"]

# Trailing comma.
# MATCH:
RUN ["a",]

# Leading comma.
# MATCH:
RUN [, "a"]

# Bad escape.
# MATCH:
RUN ["a\q"]

# Numbers are not strings.
# MATCH:
RUN [42]

# Single quotes are not JSON.
# MATCH:
RUN ['a']

# Nested arrays are not arrays of strings.
# MATCH:
RUN [["a"]]

# A well-formed array trailed by anything else: the argument as a whole is not
# valid JSON, so docker runs the line through the shell.
# MATCH:
RUN ["a"] && echo done

# MATCH:
RUN ["a"] extra

# MATCH:
RUN ["a"] # not a comment to docker

# MATCH:
RUN ["a"] \
    && echo b

# MATCH:
RUN ["a", \
     "b"] && echo c

# Exec form: not a shell command.
RUN ["a", "b"]
RUN []
RUN [ "a" ]
