options_only() {
  set -e -o pipefail
  # ok: test-bash-set-positional
  eval "$2"
}

replaced() {
  set -- "$2"
  # ruleid: test-bash-set-positional
  eval "$1"
}

run() {
  options_only "$(get_input)" safe
  replaced safe "$(get_input)"
}
