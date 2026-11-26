shifted_safe() {
  shift
  # ok: test-bash-shift-all-positional
  eval "$@"
}

shifted_tainted() {
  shift
  # ruleid: test-bash-shift-all-positional
  eval "$@"
}

run() {
  shifted_safe "$(get_input)" safe
  shifted_tainted safe "$(get_input)"
}
