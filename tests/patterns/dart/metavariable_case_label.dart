void f(int k, List<int> l) {
  // ERROR: match
  switch (k) {
    case FOO:
      a();
  }

  // ERROR: match
  switch (k) {
    case 2:
      a();
  }

  // ERROR: match
  switch (l) {
    case [1, 2]:
      a();
  }
}
