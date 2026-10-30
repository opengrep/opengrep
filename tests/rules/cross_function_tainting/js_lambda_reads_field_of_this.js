// The this of an arrow function is that of the method that defines it.
class A {
  m() {
    this.x = source();
    this.y = "safe";
    // ruleid: js_lambda_reads_field_of_this
    run(() => sink(this.x));
    // ok: js_lambda_reads_field_of_this
    run(() => sink(this.y));
  }
}
