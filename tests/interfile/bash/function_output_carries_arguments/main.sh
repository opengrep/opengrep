trim() {
  sed 's/^ *//' <<< "$1"
}
lines_of_argument() {
  while IFS= read -r line; do
    printf '%s\n' "$line"
  done < <(printf '%s\n' "$1")
}
store() {
  stored=$1
}
print_stored() {
  store "$1"
  echo "$stored"
}
run_logged() {
  if "$@"; then :; fi 2>&1 | tee -a log
}
run() {
  trimmed=$(trim "$(get_input)")
  # ruleid: test-bash-function-output-carries-arguments
  eval "$trimmed"
  lines=$(lines_of_argument "$(get_input)")
  # ruleid: test-bash-function-output-carries-arguments
  eval "$lines"
  printed=$(print_stored "$(get_input)")
  # ruleid: test-bash-function-output-carries-arguments
  eval "$printed"
  logged=$(run_logged printf '%s' "$(get_input)")
  # ruleid: test-bash-function-output-carries-arguments
  eval "$logged"
  constant=$(trim constant)
  # ok: test-bash-function-output-carries-arguments
  eval "$constant"
}
