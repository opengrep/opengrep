first() {
  # ruleid: test-bash-forward-all-positional
  eval "$1"
}

second() {
  # ok: test-bash-forward-all-positional
  eval "$2"
}

forward() {
  first "$@"
  second "$@"
}

run() {
  forward "$(get_input)" safe
}
