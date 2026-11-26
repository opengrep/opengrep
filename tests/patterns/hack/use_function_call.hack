use function A\B\c;

function g(): void {
  // ERROR:
  A\B\c();
  c();
  d();
}
