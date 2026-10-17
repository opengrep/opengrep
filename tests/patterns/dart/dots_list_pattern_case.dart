void f(Object k) {
  // ERROR: match
  switch (k) {
    case []:
      a();
  }

  // A list pattern is not a map pattern.
  switch (k) {
    case {'a': 1}:
      a();
  }
}
