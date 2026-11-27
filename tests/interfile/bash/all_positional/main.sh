relay_all() {
  # ruleid: test-bash-all-positional
  eval "$@"
}

relay_star() {
  # ruleid: test-bash-all-positional
  eval "$*"
}

log_first_relay_all() {
  echo "$1"
  # ruleid: test-bash-all-positional
  eval "$@"
}

relay_first_log_all() {
  # ok: test-bash-all-positional
  eval "$1"
  echo "$@"
}

run() {
  relay_all "$(get_input)"
  relay_star safe "$(get_input)"
  log_first_relay_all safe "$(get_input)"
  relay_first_log_all safe "$(get_input)"
}
