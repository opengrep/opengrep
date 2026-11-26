source ./lib.sh

local_relay() {
  # ruleid: test-bash-source-file
  eval "$2"
}

run() {
  relay "$(get_input)"
  quiet "constant"
  local_relay "constant" "$(get_input)"
}
