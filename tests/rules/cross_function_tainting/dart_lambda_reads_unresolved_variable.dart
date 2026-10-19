// A variable of a [for] initialiser, which naming does not resolve, read by a
// lambda: the lambda captures it.
void tainted() {
  for (var s = source(); s != null; ) {
    // ruleid: dart_lambda_reads_unresolved_variable
    var g = () => sink(s);
    g();
    break;
  }
}

void untainted() {
  for (var s = clean(); s != null; ) {
    // ok: dart_lambda_reads_unresolved_variable
    var g = () => sink(s);
    g();
    break;
  }
}
