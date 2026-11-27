[ -f ./lib.sh ] && . ./lib.sh

run() {
  relay "$(get_input)"
}
