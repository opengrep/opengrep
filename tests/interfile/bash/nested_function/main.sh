outer_constant() {
  inner_constant() {
    # ok: test-bash-nested-function
    eval "$1"
  }
  inner_constant safe
}

outer_relay() {
  inner_relay() {
    # ruleid: test-bash-nested-function
    eval "$1"
  }
  inner_relay "$1"
}

run() {
  outer_constant "$(get_input)"
  outer_relay "$(get_input)"
}
