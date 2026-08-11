FROM alpine

# MATCH:
CMD [ -f /etc/passwd ]

# MATCH:
CMD test [ 1 -eq 1 ]

# MATCH:
CMD echo "]" "[" '[' ']'

# MATCH:
CMD awk '{print $1}' | sed 's/[a-z]//g'

# MATCH:
CMD if [ -x /bin/sh ]; then echo y; fi

# MATCH:
CMD [ -f a ] \
    && [ -f b ] \
    || [ -f c ]

CMD ["a", "b"]
