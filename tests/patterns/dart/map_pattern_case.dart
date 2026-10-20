void f(Object k) {
  // ERROR: match
  switch (k) {
    case {'a': 1}:
      a();
  }

  // ERROR: match
  switch (k) {
    case {'a': 1, 'b': 2}:
      a();
  }

  // ERROR: match
  switch (k) {
    case {'b': 2, 'a': 1}:
      a();
  }

  switch (k) {
    case {'b': 2}:
      a();
  }
}
