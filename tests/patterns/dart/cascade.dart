void main() {
  var sb = StringBuffer();
  // a cascade '..' no longer crashes parsing; its operations are matchable
  // with normal method-call patterns
  // MATCH:
  sb..write('hello')..writeln('!');
}
