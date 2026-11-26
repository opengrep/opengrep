cluster_safe() {
  set -euo pipefail
  # ok: test-bash-set-option-cluster
  eval "$2"
}

cluster_tainted() {
  set -eo pipefail
  # ruleid: test-bash-set-option-cluster
  eval "$1"
}

end_of_options_alone() {
  set -
  # ok: test-bash-set-option-cluster
  eval "$2"
}

run() {
  cluster_safe "$(get_input)" safe
  cluster_tainted "$(get_input)" safe
  end_of_options_alone "$(get_input)" safe
}
