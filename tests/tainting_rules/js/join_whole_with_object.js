function array_literal(c) {
  const x = c ? source() : [source(), ""];
  // ruleid: join_whole_with_object
  sink(x[1]);
}

function object_literal(c) {
  const x = c ? source() : { a: source(), b: "" };
  // ruleid: join_whole_with_object
  sink(x.b);
}

function two_arrays(c) {
  const x = c ? ["", ""] : [source(), ""];
  // ok: join_whole_with_object
  sink(x[1]);
}
