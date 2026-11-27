use function Q\{r, s};

function g(): void {
  // ERROR:
  Q\r();
  r();
  s();
}
