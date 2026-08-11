# Comments are 'extras', so they can appear between the tokens of an array.
FROM alpine

# MATCH:
CMD ["a", "b"]

# A '#' in the middle of a string is just a character.
# MATCH:
CMD ["echo", "not-a#comment [x]"]

# MATCH:
CMD ["a", \
     # a comment between the elements
     "b"]

# MATCH:
CMD [ \
     # leading comment
     "a"]

CMD [ -f /x ] # a trailing comment on a shell command
