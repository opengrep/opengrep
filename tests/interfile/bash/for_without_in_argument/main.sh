each_word() {
  for word; do
    # ruleid: test-bash-for-without-in-argument
    eval "$word"
  done
}
each_constant() {
  for word; do
    # ok: test-bash-for-without-in-argument
    eval "$word"
  done
}
run() {
  each_word safe "$(get_input)"
  each_constant safe constant
}
