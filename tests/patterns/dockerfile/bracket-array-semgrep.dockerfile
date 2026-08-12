FROM alpine

# MATCH:
RUN ["b"]

# MATCH:
RUN ["a", "b"]

# MATCH:
RUN ["a", "b", "c"]

# MATCH:
RUN [ "a" , "b" , "c" ]

RUN ["a", "c"]
RUN [ b ]
RUN ["b" "b"]
