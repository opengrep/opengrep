void f(Object k) {
  // ERROR: match
  switch (k) {
    case {}:
      a();
  }

  // ERROR: match
  switch (k) {
    case {'a': 1}:
      a();
  }

  // A map pattern is not a list pattern.
  switch (k) {
    case []:
      a();
  }
}
