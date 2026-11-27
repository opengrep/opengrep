[ -f "$dir/lib.sh" ] && . "$dir/lib.sh"

run() {
  relay "$(get_input)"
}
