run() {
  date=$(get_input)
  stamp=$(date +%s)
  # ok: test-bash-command-namespace
  eval "echo $stamp"
  formatted=$(date "$date")
  # ruleid: test-bash-command-namespace
  eval "echo $formatted"
}
