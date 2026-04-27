FROM alpine:3.19

# Heredoc COPY with unquoted marker
COPY <<EOF /app/hello.txt
Hello, world!
EOF

# Heredoc COPY with single-quoted marker (prevents variable expansion in body)
COPY <<'EOF' /app/script.sh
#!/bin/bash
echo "not $expanded"
EOF

# Heredoc COPY preceded by a simple RUN (regression guard: no double-registration
# of the heredoc marker when a prior instruction runs through error recovery)
RUN echo hello

COPY <<'EOF' /app/config.cfg
key=value
# comment inside heredoc body
EOF

# RUN heredoc (already worked, regression guard)
RUN <<'SCRIPT'
#!/bin/sh
set -e
echo done
SCRIPT
