FROM ubuntu:22.04

# RUN with a multi-line single-quoted shell string followed by a heredoc COPY.
# Tree-sitter greedily folds the COPY and its <<'EOF' marker into the shell_fragment
# of the RUN command; without the Here_marker_pat_ea34a52 fix the OCaml layer would
# crash with an empty-list assertion in unsafe_concat_tokens.
RUN mkdir -p /var/www/html && \
    echo '<!DOCTYPE html>
<html>
<head><title>HTTP/TLS Lab</title></head>
<body>
    <h1>HTTP/TLS Lab</h1>
    <p>Month 7: HTTP/TLS &amp; Security</p>
    <script>console.log("JavaScript loaded");</script>
</body>
</html>' > /var/www/html/index.html

COPY <<'EOF' /workspace/scripts/capture.sh
#!/bin/bash

INTERFACE=${INTERFACE:-"eth0"}
OUTPUT=${OUTPUT:-"/workspace/captures/capture.pcap"}
FILTER=${FILTER:-""}

echo "Starting capture on $INTERFACE..."
if [ -n "$FILTER" ]; then
    tcpdump -i $INTERFACE -w $OUTPUT $FILTER
else
    tcpdump -i $INTERFACE -w $OUTPUT
fi

echo "Capture saved to $OUTPUT"
EOF
