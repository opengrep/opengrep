use type A\B\Foo;

function g(): void {
  // ERROR:
  A\B\Foo();
  Foo();
}
