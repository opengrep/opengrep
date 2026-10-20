void f(Object k) {
  // ERROR: match
  switch (k) {
    case {}:
      a();
  }

  // A rule `{}` matches any map pattern: a map pattern matches a map with
  // other entries too.
  // ERROR: match
  switch (k) {
    case {'a': 1}:
      a();
  }
}
