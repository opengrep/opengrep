FROM debian

# ok: taint-command-namespace
RUN date=$(get_input); stamp=$(date +%s); eval "echo $stamp"

# ruleid: taint-command-namespace
RUN date=$(get_input); formatted=$(date "$date"); eval "echo $formatted"
