void f(List<int> k) {
  // ERROR: match
  switch (k) {
    case [1, 2]:
      a();
  }

  // ERROR: match
  switch (k) {
    case [CMD]:
      a();
  }

  // ERROR: match
  switch (k) {
    case [1, ...]:
      a();
  }

  switch (k) {
    case 1:
      a();
  }
}
