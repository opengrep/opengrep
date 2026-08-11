# Exec-form arrays must survive '[' also being a legal start of a shell command.
FROM alpine

# MATCH:
RUN ["echo", "hello"]

# MATCH:
RUN [ "a" , "b" ]

# MATCH:
RUN []

# MATCH:
RUN [  ]

# Escapes of every shape the JSON grammar allows.
# MATCH:
RUN ["a\"b", "c\\d", "e\/f", "g\bh", "i\fj", "k\nl", "m\rn", "o\tp", "qér"]

# Brackets, commas and quotes inside a string are just characters.
# MATCH:
RUN ["echo", "[ok] a,b \"c\""]

# Split over a line continuation.
# MATCH:
RUN ["a", \
     "b"]

# Trailing blanks and a trailing continuation don't end the exec form.
# MATCH:
RUN ["a"]	

# MATCH:
RUN ["a"] \

# Not arrays.
RUN [ -f /x ]
RUN ["a" "b"]
RUN ["a"] && echo b
