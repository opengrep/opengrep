void f(Object k) {
  // ERROR: match
  switch (k) {
    case Point(x: 1, y: 2):
      c();
  }

  // ERROR: match
  switch (k) {
    case 2:
      d();
  }

  switch (k) {
    case 3:
      e();
  }
}
