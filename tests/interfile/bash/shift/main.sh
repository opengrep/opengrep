shifted_safe() {
  shift
  # ok: test-bash-shift
  eval "$1"
}

shifted_tainted() {
  shift
  # ruleid: test-bash-shift
  eval "$1"
}

run() {
  shifted_safe "$(get_input)" safe
  shifted_tainted safe "$(get_input)"
}
