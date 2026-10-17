void f(Object k) {
  // ERROR: match
  switch (k) {
    case {}:
      a();
  }

  // A rule `{}` matches the empty map pattern only.
  switch (k) {
    case {'a': 1}:
      a();
  }
}
