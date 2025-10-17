function passThrough(value) {
  return value;
}

function main() {
  const input = source();
  // ruleid: javascript_return_taint
  sink(passThrough(input));
}
