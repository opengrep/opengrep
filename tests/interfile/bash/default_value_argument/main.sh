first_or_default() {
  # ruleid: test-bash-default-value-argument
  eval "${1:-default}"
}
second_or_default() {
  # ok: test-bash-default-value-argument
  eval "${2:-default}"
}
run() {
  first_or_default "$(get_input)" safe
  second_or_default "$(get_input)" safe
}
