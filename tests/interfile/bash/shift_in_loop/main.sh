each_tainted() {
  while [ $# -gt 0 ]; do
    # ruleid: test-bash-shift-in-loop
    eval "$1"
    shift
  done
}

each_constant() {
  while [ $# -gt 0 ]; do
    # ok: test-bash-shift-in-loop
    eval "$1"
    shift
  done
}

run() {
  each_tainted safe "$(get_input)"
  each_constant safe constant
}
