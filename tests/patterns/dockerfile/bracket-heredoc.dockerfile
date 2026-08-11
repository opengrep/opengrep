# Heredoc bodies are opaque to the dockerfile grammar and must stay that way
# now that '[' can start a shell fragment.
FROM alpine

# MATCH:
RUN <<EOF
[ -f /etc/os-release ] && echo yes
grep 'Python [0-9]*' /etc/issue
echo ["not", "an", "array"]
EOF

# MATCH:
RUN [ -f /tmp/x ] && cat <<EOF
[ still inside the heredoc ]
EOF

# MATCH:
RUN <<-EOF
	[ indented heredoc ]
EOF

COPY <<EOF /tmp/f
[ a heredoc attached to COPY ]
EOF
