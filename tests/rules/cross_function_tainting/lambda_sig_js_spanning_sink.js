// A sink match that spans a lambda's definition belongs to the code around
// the lambda: the finding is the inner index, not the whole assignment.
table[key] = function () {
  return run(function () {
    // ruleid: lambda-sig-js-spanning-sink
    use(source()[name]);
    // ok: lambda-sig-js-spanning-sink
    use(other[name]);
  });
};
