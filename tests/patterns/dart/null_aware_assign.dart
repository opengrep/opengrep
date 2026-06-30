void main() {
  var config;
  // null-aware assignment '??=' no longer crashes and is matchable
  // MATCH:
  config ??= defaultConfig();
}
