void main(y) {
  // type arguments on a generic constructor are now kept (and matchable)
  // MATCH:
  var b = Bar<String, int>(y);
}
