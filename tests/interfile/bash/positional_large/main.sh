large() {
  echo "$99999999999999999999"
}

relay() {
  # ruleid: test-bash-positional-large
  eval "$1"
}

quiet() {
  # ok: test-bash-positional-large
  eval "$1"
}

run() {
  relay "$(get_input)"
  quiet constant
}
