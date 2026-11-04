// A closure reads the fields of the [this] of the method that made it.
class B {
  get() {
    return () => this.x;
  }
}
function main() {
  const b = new B();
  b.x = source();
  const f = b.get();
  // ruleid: js_closure_reads_this_of_its_maker
  sink(f());
  const c = new B();
  c.x = "safe";
  const g = c.get();
  // ok: js_closure_reads_this_of_its_maker
  sink(g());
}
