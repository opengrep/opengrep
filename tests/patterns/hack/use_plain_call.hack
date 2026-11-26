use A\B\c;

function g(): void {
  // ERROR:
  A\B\c();
  c();
}
