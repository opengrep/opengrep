first() {
  # ruleid: test-bash-positional-parameter
  eval "$1"
}

second() {
  # ok: test-bash-positional-parameter
  eval "$2"
}

run() {
  first "$(get_input)" safe
  second "$(get_input)" safe
}
