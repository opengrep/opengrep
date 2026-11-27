use function W\x, W\y;

function g(): void {
  // ERROR:
  W\y();
  y();
  x();
}
