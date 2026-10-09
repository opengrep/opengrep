// A closure in [helper] writes [acc], a local of [helper]: that write is not
// an effect of [helper]'s signature, a caller cannot observe it. The write of
// the module-level [current] is.
current = null;

function helper(x) {
  let acc;
  const set = (v) => {
    acc = v;
  };
  set(x);
  current = x;
  return acc;
}

// A use of the rule's source and sink, without which the rule does not apply
// to the file.
function go() {
  sink(helper(sourceA()));
}
