void f(Object k) {
  // ERROR: match
  switch (k) {
    case Point(x: 1):
      a();
  }

  // ERROR: match
  switch (k) {
    case Point(x: 1, y: 2):
      a();
  }

  // ERROR: match
  switch (k) {
    case Point():
      a();
  }

  switch (k) {
    case Line(x: 1):
      a();
  }
}
